module Test.Main where

import Prelude

import Control.Monad.Free (Free)
import Data.Either (Either(..), hush, isRight)
import Data.Foldable (length)
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe(..), isJust)
import Data.Time.Duration (Milliseconds(..))
import Debug (spy, trace)
import Effect.Class (liftEffect)
import Erl.Atom (atom)
import Erl.Data.List (List, head, (:), filter)
import Erl.Data.List as List
import Erl.Data.Map as Map
import Erl.Kernel.Application as Application
import Erl.Kernel.Erlang (sleep)
import Erl.Oracle.AvailabilityDomain (defaultListAvailabilityDomainRequest, listAvailabilityDomains)
import Erl.Oracle.CapacityReservation (defaultListCapacityReservationRequest, listCapacityReservations)
import Erl.Oracle.Compartments (defaultListCompartmentsRequest, listCompartments)
import Erl.Oracle.Images (defaultListImagesRequest, listImages)
import Erl.Oracle.Instance (defaultLaunchInstanceRequest, defaultListInstancesRequest, defaultTerminateInstanceRequest, launchInstance, listInstances, terminateInstance)
import Erl.Oracle.Shape (defaultListShapesRequest, listShapes)
import Erl.Oracle.ShapeCompatibility (defaultListImageShapeCompatibilityRequest, listCompatibleShapes)
import Erl.Oracle.Subnet (createSubnet, defaultCreateSubnetRequest, defaultDeleteSubnetRequest, defaultGetSubnetRequest, defaultListSubnetsRequest, deleteSubnet, getSubnet, listSubnets)
import Erl.Oracle.Types.AvailabilityDomain (AvailabilityDomain)
import Erl.Oracle.Types.Common (AvailabilityDomainId, CompartmentId(..), ImageId(..), Shape(..), OciProfile)
import Erl.Oracle.Types.Instance (InstanceLifecycleState(..))
import Erl.Oracle.Types.Subnet (SubnetDetails)
import Erl.Oracle.Types.VirtualCloudNetwork (VcnDetails)
import Erl.Oracle.VirtualCloudNetwork (createVcn, defaultCreateVcnRequest, defaultDeleteVcnRequest, defaultListVcnsRequest, deleteVcn, listVcns)
import Erl.Oracle.VirtualNetworkInterface (defaultGetVnic, getVnic)
import Erl.Oracle.VirtualNetworkInterfaceAttachment (defaultListVnicAttachments, listVnicAttachments)
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
  suite "list compartments" do
    test "Can parse response" do
      void $ Application.ensureAllStarted $ atom "erlexec"
      unsafeRunProcessM
        $ do
            -- Look at the parent compartment
            actual <- liftEffect $ listCompartments $ defaultListCompartmentsRequest profile $ Just (CompartmentId "ocid1.tenancy.oc1..aaaaaaaajycivti4bmq4kignjlabrv2egvp7abj676tzkeefavboxavstx7a")
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
            actual <- liftEffect $ listShapes $ defaultListShapesRequest profile Nothing
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
            actual <- liftEffect $ listImages $ defaultListImagesRequest profile Nothing
            case actual of
              Right actualList -> do
                liftEffect $ assertTrue' "At least 1 image offerings" $ (length actualList) >= 1
              Left _ -> do
                liftEffect $ assertEqual { expected: Right List.nil, actual }
  suite "list instances" do
    test "Can parse response" do
      void $ Application.ensureAllStarted $ atom "erlexec"
      unsafeRunProcessM
        $ do
            actual <- liftEffect $ listInstances $ defaultListInstancesRequest profile Nothing
            case actual of
              Right actualList -> do
                liftEffect $ assertTrue' "No running instances" $ (length $ filter (\{ lifecycleState } -> lifecycleState /= Terminated) actualList) == 0
              Left _ -> do
                liftEffect $ assertEqual { expected: Right List.nil, actual }
  suite "list availability domains" do
    test "Can parse response" do
      void $ Application.ensureAllStarted $ atom "erlexec"
      unsafeRunProcessM
        $ do
            actual <- liftEffect $ listAvailabilityDomains $ defaultListAvailabilityDomainRequest profile Nothing
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
            actual <- liftEffect $ listCapacityReservations $ defaultListCapacityReservationRequest profile Nothing
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
            actual <- liftEffect $ listCompatibleShapes $ defaultListImageShapeCompatibilityRequest profile Nothing (ImageId "ocid1.image.oc1.uk-london-1.aaaaaaaayc2h4cb5lcvcjh3dtm7m6mqkmtkz2rbz4vrur27gqzniw2raxevq")
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
            actual <- liftEffect $ createVcn $ (defaultCreateVcnRequest profile Nothing)
              { cidrBlocks = Just $ "192.168.0.0/24" : List.nil
              , displayName = Just "unit test vcn"
              , dnsLabel = Just "unittestvcn"
              }

            case actual of
              Right vcn -> do
                liftEffect $ assertTrue' "VCN has been created" $ isRight actual
                deleteResult <- liftEffect $ deleteVcn $ defaultDeleteVcnRequest profile Nothing vcn.id
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
              newVcn <- liftEffect $ createVcn $ (defaultCreateVcnRequest profile Nothing)
                { cidrBlocks = Just $ "192.168.0.0/24" : List.nil
                , displayName = Just "unit test vcn"
                , dnsLabel = Just "unittestvcn"
                }

              liftEffect $ assertTrue' "VCN has been created" $ isRight newVcn

              (vcns :: Either (NonEmptyList ForeignError) (List VcnDetails)) <- liftEffect $ listVcns $ defaultListVcnsRequest profile Nothing

              let
                vcns' :: Maybe (List VcnDetails)
                vcns' = hush vcns

              liftEffect $ assertTrue' "VCN list" $ isJust $ vcns'

              let
                maybeVcn = case vcns' of
                  Just vcnList -> do
                    case head vcnList of
                      Just v -> Just v.id
                      Nothing -> Nothing
                  Nothing -> Nothing

              case maybeVcn of
                Just vcnId -> do
                  actual <- liftEffect $ createSubnet $ ((defaultCreateSubnetRequest profile Nothing "192.168.0.0/24" vcnId) { dnsLabel = Just "utsubnet" })
                    { displayName = Just "unit test subnet"
                    }
                  liftEffect $ assertTrue' "Subnet was created" $ isRight actual

                  case actual of
                    Right s -> do
                      createdSubnet <- liftEffect $ getSubnet $ (defaultGetSubnetRequest profile Nothing s.id)
                      liftEffect $ assertTrue' "Subnet retrieved by id" $ isRight createdSubnet
                    Left _ ->
                      pure $ unit

                Nothing ->
                  liftEffect $ assertTrue' "Subnet was not created" $ isJust maybeVcn

              (subnets :: Either (NonEmptyList ForeignError) (List SubnetDetails)) <- liftEffect $ listSubnets $ (defaultListSubnetsRequest profile Nothing) { displayName = Just "unit test subnet" }

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

              (availabilityDomains :: Either (NonEmptyList ForeignError) (List AvailabilityDomain)) <- liftEffect $ listAvailabilityDomains $ defaultListAvailabilityDomainRequest profile Nothing

              let
                availabilityDomains' :: Maybe (List AvailabilityDomain)
                availabilityDomains' = hush availabilityDomains

                maybeAvailabilityDomain :: Maybe AvailabilityDomainId
                maybeAvailabilityDomain = case availabilityDomains' of
                  Just s -> do
                    let
                      firstDomain = head $ List.reverse s
                    case firstDomain of
                      Just ad -> ad.id
                      Nothing -> Nothing
                  Nothing ->
                    Nothing

              case maybeSubnet, maybeAvailabilityDomain, maybeVcn of
                Just subnet, Just availabilityDomain, Just vcnId -> do
                  let
                    metadata = Map.insert "test" "key" Map.empty
                    extendedMetadata = Map.insert "id3as.userData"
                      ( Map.insert "dockerLabel" "norsk-worker:v1.0.341-main-arm64"
                          $ (Map.insert "mappedPort" "1:2")
                          $ (Map.insert "workerServicePort" "2")
                          $ (Map.insert "nodeId" "foo")
                          $ (Map.insert "json" "foo")
                          $ Map.empty
                      )
                      Map.empty

                  createdInstance <- liftEffect $ launchInstance $ ((defaultLaunchInstanceRequest profile Nothing availabilityDomain (Shape "VM.Standard.E2.1.Micro") subnet) { extendedMetadata = Just $ extendedMetadata })
                    { displayName = Just "unit test instance"
                    , imageId = Just $ ImageId "ocid1.image.oc1.uk-london-1.aaaaaaaazngdzjtqmduhr2w3gzijcwyvtahaucuqyj2bxxp2lwyvxk5oanfa"
                    , metadata = Just $ metadata
                    }

                  liftEffect $ assertTrue' "Instance was launched " $ isRight createdInstance

                  case createdInstance of
                    Right inst -> do

                      -- Need provisioning to have started before NICs are visible
                      liftEffect $ sleep $ Milliseconds 8000.0

                      vnicDetails <- liftEffect $ listVnicAttachments $ (defaultListVnicAttachments profile Nothing) { instanceId = Just inst.id }
                      void $ liftEffect $ assertTrue' "Vnic attachment exists" $ isRight $ vnicDetails

                      case vnicDetails of
                        Right vnicDetails' -> do
                          case head vnicDetails' of
                            Just vnicAttachment -> do
                              case vnicAttachment.vnicId of
                                Just vnicId -> do
                                  vnic <- liftEffect $ getVnic $ (defaultGetVnic profile Nothing vnicId)
                                  void $ liftEffect $ assertTrue' "Vnic exists" $ isRight vnic
                                  case vnic of
                                    Right { privateIp } ->
                                      liftEffect $ assertTrue' "Vnic has private ip" $ isJust privateIp
                                    _ ->
                                      void $ liftEffect $ assertTrue' "No vnic" false
                                Nothing ->
                                  void $ liftEffect $ assertTrue' "No vnicId" false
                            Nothing ->
                              void $ liftEffect $ assertTrue' "No vnic attachments" false
                        Left _ -> do
                          void $ liftEffect $ assertTrue' "Error getting vnic attachments" false

                      terminatedInstance <- liftEffect $ terminateInstance $ defaultTerminateInstanceRequest profile Nothing inst.id
                      void $ liftEffect $ assertTrue' "Terminated instance" $ isRight $ spy "Terminated instance result" terminatedInstance
                      void $ liftEffect $ sleep $ Milliseconds 80000.0
                      deletedSubnet <- liftEffect $ deleteSubnet $ defaultDeleteSubnetRequest profile Nothing subnet
                      void $ liftEffect $ assertTrue' "Deleted subnet" $ isRight $ spy "Deleted subnet result" deletedSubnet
                      void $ liftEffect $ deleteVcn $ defaultDeleteVcnRequest profile Nothing vcnId
                    Left _ -> do
                      liftEffect $ assertTrue' "Instance was not created " false
                  liftEffect $ assertTrue' "Instance was launched " true
                _, _, _ ->
                  liftEffect $ assertTrue' "Instance was not created " false

profile :: OciProfile
profile =
  { defaultCompartment: CompartmentId "ocid1.compartment.oc1..aaaaaaaaf7prbnbobebhupne4afstgmtkfebpagkt4rhrphqudumceda6tgq"
  , configFile: "~/.oci/config"
  , ociProfileName: "id3as"
  }