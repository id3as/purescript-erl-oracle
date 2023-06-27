module Test.Main where

import Prelude

import Control.Monad.Free (Free)
import Data.Either (Either(..))
import Data.Foldable (length)
import Data.Maybe (Maybe(..))
import Debug (spy)
import Effect.Class (liftEffect)
import Erl.Atom (atom)
import Erl.Data.List (List)
import Erl.Data.List as List
import Erl.Kernel.Application as Application
import Erl.Oracle (listAvailabilityDomains, listCapacityReservations, listCompartments, listCompatibleShapes, listImages, listShapes)
import Erl.Oracle.Types (CompartmentId(..), ImageId(..))
import Erl.Process (unsafeRunProcessM)
import Erl.Test.EUnit (TestF, TestSet, collectTests, suite, test)
import Test.Assert (assertEqual, assertTrue')

main_test_ :: List TestSet
main_test_ =
  collectTests ociTests

-- These tests won't work unless the oci CLI is already configured
ociTests :: Free TestF Unit
ociTests = do
  let
    compartment = Just $ CompartmentId "ocid1.tenancy.oc1..aaaaaaaaj3cbwlxdi7tewdo2zikxrp3qiatz3tx6l2vj6bpbvr2wp5u5lrja"
  -- suite "list compartments" do
  --   test "Can parse response" do
  --     void $ Application.ensureAllStarted $ atom "erlexec"
  --     unsafeRunProcessM
  --       $ do
  --           actual <- liftEffect $ listCompartments { compartment }
  --           case actual of
  --             Right actualList -> do
  --               liftEffect $ assertTrue' "At least 1 type offerings" $ (length actualList) >= 1
  --             Left _ -> do
  --               liftEffect $ assertEqual { expected: Right List.nil, actual }
  -- suite "list shapes" do
  --   test "Can parse response" do
  --     void $ Application.ensureAllStarted $ atom "erlexec"
  --     unsafeRunProcessM
  --       $ do
  --           actual <- liftEffect $ listShapes { compartment }
  --           case actual of
  --             Right actualList -> do
  --               liftEffect $ assertTrue' "At least 1 shape offerings" $ (length actualList) >= 1
  --             Left _ -> do
  --               liftEffect $ assertEqual { expected: Right List.nil, actual }
  -- suite "list images" do
  --   test "Can parse response" do
  --     void $ Application.ensureAllStarted $ atom "erlexec"
  --     unsafeRunProcessM
  --       $ do
  --           actual <- liftEffect $ listImages { compartment }
  --           case actual of
  --             Right actualList -> do
  --               liftEffect $ assertTrue' "At least 1 image offerings" $ (length actualList) >= 1
  --             Left _ -> do
  --               liftEffect $ assertEqual { expected: Right List.nil, actual }
  -- suite "list availability domains" do
  --   test "Can parse response" do
  --     void $ Application.ensureAllStarted $ atom "erlexec"
  --     unsafeRunProcessM
  --       $ do
  --           actual <- liftEffect $ listAvailabilityDomains { compartment }
  --           case actual of
  --             Right actualList -> do
  --               liftEffect $ assertTrue' "At least 1 image offerings" $ (length actualList) >= 1
  --             Left _ -> do
  --               liftEffect $ assertEqual { expected: Right List.nil, actual }
  suite "list capacity reservations" do
    test "Can parse response" do
      void $ Application.ensureAllStarted $ atom "erlexec"
      unsafeRunProcessM
        $ do
            actual <- liftEffect $ listCapacityReservations { compartment, availabilityDomain: Nothing }
            case actual of
              Right actualList -> do
                liftEffect $ assertTrue' "At least 1 image offerings" $ (length actualList) >= 1
              Left _ -> do
                liftEffect $ assertEqual { expected: Right List.nil, actual }
-- suite "list compatible shapes" do
--   test "Can parse response" do
--     void $ Application.ensureAllStarted $ atom "erlexec"
--     unsafeRunProcessM
--       $ do
--           -- Canonical-Ubuntu-22.04-aarch64-2023.05.19-0
--           actual <- liftEffect $ listCompatibleShapes { imageId: ImageId "ocid1.image.oc1.uk-london-1.aaaaaaaayc2h4cb5lcvcjh3dtm7m6mqkmtkz2rbz4vrur27gqzniw2raxevq" }
--           case actual of
--             Right actualList -> do
--               liftEffect $ assertTrue' "At least 1 compatible shape" $ (length actualList) >= 1
--             Left _ -> do
--               liftEffect $ assertEqual { expected: Right List.nil, actual }

