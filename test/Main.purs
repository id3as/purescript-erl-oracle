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
import Erl.Oracle (listCompartments, listShapes)
import Erl.Process (unsafeRunProcessM)
import Erl.Test.EUnit (TestF, TestSet, collectTests, suite, test)
import Test.Assert (assertEqual, assertTrue')

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
            actual <- liftEffect $ listCompartments { compartment: Just "ocid1.tenancy.oc1..aaaaaaaaj3cbwlxdi7tewdo2zikxrp3qiatz3tx6l2vj6bpbvr2wp5u5lrja" }
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
            actual <- liftEffect $ listShapes { compartment: Just "ocid1.tenancy.oc1..aaaaaaaaj3cbwlxdi7tewdo2zikxrp3qiatz3tx6l2vj6bpbvr2wp5u5lrja" }
            case actual of
              Right actualList -> do
                liftEffect $ assertTrue' "At least 1 shape offerings" $ (length actualList) >= 1
              Left _ -> do
                liftEffect $ assertEqual { expected: Right List.nil, actual }

