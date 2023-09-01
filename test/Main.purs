module Test.Main where

import Prelude

import Control.Monad.Free (Free)
import Data.Either (Either(..), hush, isRight)
import Data.Foldable (length)
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe(..), isJust)
import Data.Time.Duration (Milliseconds(..))
import Effect.Class (liftEffect)
import Erl.Atom (atom)
import Erl.Data.List (List, head, (:))
import Erl.Data.List as List
import Erl.Kernel.Application as Application
import Erl.Kernel.Erlang (sleep)
import Erl.Oracle.AvailabilityDomain (defaultListAvailabilityDomainRequest, listAvailabilityDomains)
import Erl.Oracle.Types.AvailabilityDomain (AvailabilityDomain)
import Erl.Oracle.CapacityReservation (defaultListCapacityReservationRequest, listCapacityReservations)
import Erl.Oracle.Compartments (defaultListCompartmentsRequest, listCompartments)
import Erl.Oracle.Images (defaultListImagesRequest, listImages)
import Erl.Oracle.Instance (defaultLaunchInstanceRequest, defaultTerminateInstanceRequest, launchInstance, terminateInstance)
import Erl.Oracle.Shape (defaultListShapesRequest, listShapes)
import Erl.Oracle.ShapeCompatibility (defaultListImageShapeCompatibilityRequest, listCompatibleShapes)
import Erl.Oracle.Subnet (createSubnet, defaultCreateSubnetRequest, defaultDeleteSubnetRequest, defaultListSubnetsRequest, deleteSubnet, listSubnets)
import Erl.Oracle.Types.Subnet (SubnetDetails)
import Erl.Oracle.Types.Common (AvailabilityDomainId, CompartmentId(..), ImageId(..), Shape(..))
import Erl.Oracle.VirtualCloudNetwork (createVcn, defaultCreateVcnRequest, defaultDeleteVcnRequest, defaultListVcnsRequest, deleteVcn, listVcns)
import Erl.Oracle.Types.VirtualCloudNetwork (VcnDetails)
import Erl.Process (unsafeRunProcessM)
import Erl.Test.EUnit (TestF, TestSet, collectTests, suite, test, timeout)
import Foreign (ForeignError)
import Test.Assert (assertEqual, assertEqual', assertTrue')

main_test_ :: List TestSet
main_test_ =
  collectTests ociTests

-- These tests won't work unless the oci CLI is already configured
ociTests :: Free TestF Unit
ociTests = do
  let
    -- You'll want to use your own tenancy
    compartment = CompartmentId "ocid1.tenancy.oc1..aaaaaaaaj3cbwlxdi7tewdo2zikxrp3qiatz3tx6l2vj6bpbvr2wp5u5lrja"
  suite "list compartments" do
    test "Can parse response" do
      void $ Application.ensureAllStarted $ atom "erlexec"
      unsafeRunProcessM
        $ do
            actual <- liftEffect $ listCompartments defaultListCompartmentsRequest
            case actual of
              Right actualList -> do
                liftEffect $ assertTrue' "At least 1 type offerings" $ (length actualList) >= 1
              Left _ -> do
                liftEffect $ assertEqual { expected: Right List.nil, actual }
  suite "list shapes" do
    test "Can parse response" do
      void $ Application.ensureAllStarted $ atom "erlexec"
      unsafeRunProcessM
        $ do
            actual <- liftEffect $ listShapes defaultListShapesRequest
            case actual of
              Right actualList -> do
                liftEffect $ assertTrue' "At least 1 shape offerings" $ (length actualList) >= 1
              Left _ -> do
                liftEffect $ assertEqual { expected: Right List.nil, actual }
  suite "list images" do
    test "Can parse response" do
      void $ Application.ensureAllStarted $ atom "erlexec"
      unsafeRunProcessM
        $ do
            actual <- liftEffect $ listImages defaultListImagesRequest
            case actual of
              Right actualList -> do
                liftEffect $ assertTrue' "At least 1 image offerings" $ (length actualList) >= 1
              Left _ -> do
                liftEffect $ assertEqual { expected: Right List.nil, actual }
  suite "list availability domains" do
    test "Can parse response" do
      void $ Application.ensureAllStarted $ atom "erlexec"
      unsafeRunProcessM
        $ do
            actual <- liftEffect $ listAvailabilityDomains $ defaultListAvailabilityDomainRequest
            case actual of
              Right actualList -> do
                liftEffect $ assertTrue' "At least 1 image offerings" $ (length actualList) >= 1
              Left _ -> do
                liftEffect $ assertEqual { expected: Right List.nil, actual }
  suite "list capacity reservations" do
    test "Can parse response" do
      void $ Application.ensureAllStarted $ atom "erlexec"
      unsafeRunProcessM
        $ do
            actual <- liftEffect $ listCapacityReservations defaultListCapacityReservationRequest
            case actual of
              Right actualList -> do
                liftEffect $ assertTrue' "No reservations by default" $ (length actualList) == 0
              Left _ -> do
                liftEffect $ assertEqual { expected: Right List.nil, actual }
  suite "list compatible shapes" do
    test "Can parse response" do
      void $ Application.ensureAllStarted $ atom "erlexec"
      unsafeRunProcessM
        $ do
            -- Canonical-Ubuntu-22.04-aarch64-2023.05.19-0
            actual <- liftEffect $ listCompatibleShapes $ defaultListImageShapeCompatibilityRequest $ ImageId "ocid1.image.oc1.uk-london-1.aaaaaaaayc2h4cb5lcvcjh3dtm7m6mqkmtkz2rbz4vrur27gqzniw2raxevq"
            case actual of
              Right actualList -> do
                liftEffect $ assertTrue' "At least 1 compatible shape" $ (length actualList) >= 1
              Left _ -> do
                liftEffect $ assertEqual { expected: Right List.nil, actual }

  suite "create instance" do
    test "Create vcn" do
      void $ Application.ensureAllStarted $ atom "erlexec"
      unsafeRunProcessM
        $ do
            actual <- liftEffect $ createVcn $ (defaultCreateVcnRequest compartment)
              { cidrBlocks = Just $ "192.168.0.0/24" : List.nil
              , displayName = Just "unit test vcn"
              , dnsLabel = Just "unittestvcn"
              }

            case actual of
              Right vcn -> do
                liftEffect $ assertTrue' "VCN has been created" $ isRight actual
                deleteResult <- liftEffect $ deleteVcn $ defaultDeleteVcnRequest vcn.id
                case deleteResult of
                  Right deleted ->
                    liftEffect $ assertEqual' "VCN has been deleted" { actual: deleted, expected: { success: true } }
                  Left _ -> liftEffect $ assertTrue' "VCN has been deleted" $ isRight deleteResult

              Left _ ->
                liftEffect $ assertTrue' "VCN has been created" $ isRight actual

  suite "Create subnet and instance and then tidy up" do
    timeout 120000 do
      test "Create subnet and instance and then tidy up (2m timeout)" do
        void $ Application.ensureAllStarted $ atom "erlexec"
        unsafeRunProcessM
          $ do
              (vcns :: Either (NonEmptyList ForeignError) (List VcnDetails)) <- liftEffect $ listVcns $ defaultListVcnsRequest compartment

              let
                vcns' :: Maybe (List VcnDetails)
                vcns' = hush vcns

              liftEffect $ assertTrue' "VCN list" $ isJust $ vcns'

              let
                maybeVcn = case vcns' of
                  Just vcnList -> do
                    let
                      firstVcn = head vcnList
                    case firstVcn of
                      Just v ->
                        Just v.id
                      Nothing ->
                        Nothing
                  Nothing ->
                    Nothing

              case maybeVcn of
                Just vcnId -> do
                  actual <- liftEffect $ createSubnet $ (defaultCreateSubnetRequest "192.168.0.0/24" vcnId)
                    { displayName = Just "unit test subnet"
                    }
                  liftEffect $ assertTrue' "Subnet was created" $ isRight actual
                Nothing ->
                  liftEffect $ assertTrue' "Subnet was not created" $ isJust maybeVcn

              (subnets :: Either (NonEmptyList ForeignError) (List SubnetDetails)) <- liftEffect $ listSubnets $ (defaultListSubnetsRequest compartment) { displayName = Just "unit test subnet" }

              let
                subnets' :: Maybe (List SubnetDetails)
                subnets' = hush subnets

                maybeSubnet = case subnets' of
                  Just s -> do
                    let
                      firstSubnet = head s
                    case firstSubnet of
                      Just subnet -> Just subnet.id
                      Nothing -> Nothing
                  Nothing ->
                    Nothing

              liftEffect $ assertTrue' "Subnet was not created" $ isJust maybeSubnet

              (availabilityDomains :: Either (NonEmptyList ForeignError) (List AvailabilityDomain)) <- liftEffect $ listAvailabilityDomains defaultListAvailabilityDomainRequest

              let
                availabilityDomains' :: Maybe (List AvailabilityDomain)
                availabilityDomains' = hush availabilityDomains

                maybeAvailabilityDomain :: Maybe AvailabilityDomainId
                maybeAvailabilityDomain = case availabilityDomains' of
                  Just s -> do
                    let
                      firstDomain = head s
                    case firstDomain of
                      Just ad -> ad.id
                      Nothing -> Nothing
                  Nothing ->
                    Nothing

              case maybeSubnet, maybeAvailabilityDomain of
                Just subnet, Just availabilityDomain -> do
                  createdInstance <- liftEffect $ launchInstance $ (defaultLaunchInstanceRequest availabilityDomain compartment (Shape "VM.Standard.E2.1.Micro") subnet)
                    { displayName = Just "unit test instance"
                    , imageId = Just $ ImageId "ocid1.image.oc1.uk-london-1.aaaaaaaaxraojqdm5bt764bf34uxxahjl6h3u6cx4lu264kqm7xnvhhzwh5q"
                    }

                  liftEffect $ assertTrue' "Instance was launched " $ isRight createdInstance

                  case createdInstance of
                    Right inst -> do
                      terminatedInstance <- liftEffect $ terminateInstance $ defaultTerminateInstanceRequest inst.id
                      void $ liftEffect $ assertTrue' "Terminated instance" $ isRight terminatedInstance
                      void $ liftEffect $ sleep $ Milliseconds 60000.0
                      deletedSubnet <- liftEffect $ deleteSubnet $ defaultDeleteSubnetRequest subnet
                      void $ liftEffect $ assertTrue' "Deleted subnet" $ isRight deletedSubnet
                    Left _ -> do
                      liftEffect $ assertTrue' "Instance was not created " false
                  liftEffect $ assertTrue' "Instance was launched " true
                _, _ ->
                  liftEffect $ assertTrue' "Instance was not created " false

