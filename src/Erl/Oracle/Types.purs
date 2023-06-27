module Erl.Oracle.Types where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Simple.JSON (class ReadForeign, class WriteForeign, class WriteForeignKey)

newtype CompartmentId = CompartmentId String

derive newtype instance Eq CompartmentId
derive newtype instance Ord CompartmentId
derive newtype instance ReadForeign CompartmentId
derive newtype instance WriteForeign CompartmentId
derive newtype instance WriteForeignKey CompartmentId
derive instance Newtype CompartmentId _
derive instance Generic CompartmentId _
instance Show CompartmentId where
  show = genericShow

newtype Shape = Shape String

derive newtype instance Eq Shape
derive newtype instance Ord Shape
derive newtype instance ReadForeign Shape
derive newtype instance WriteForeign Shape
derive newtype instance WriteForeignKey Shape
derive instance Newtype Shape _
derive instance Generic Shape _
instance Show Shape where
  show = genericShow

newtype ImageId = ImageId String

derive newtype instance Eq ImageId
derive newtype instance Ord ImageId
derive newtype instance ReadForeign ImageId
derive newtype instance WriteForeign ImageId
derive newtype instance WriteForeignKey ImageId
derive instance Newtype ImageId _
derive instance Generic ImageId _
instance Show ImageId where
  show = genericShow

newtype AvailabilityDomainId = AvailabilityDomainId String

derive newtype instance Eq AvailabilityDomainId
derive newtype instance Ord AvailabilityDomainId
derive newtype instance ReadForeign AvailabilityDomainId
derive newtype instance WriteForeign AvailabilityDomainId
derive newtype instance WriteForeignKey AvailabilityDomainId
derive instance Newtype AvailabilityDomainId _
derive instance Generic AvailabilityDomainId _
instance Show AvailabilityDomainId where
  show = genericShow

newtype CapacityReservationId = CapacityReservationId String

derive newtype instance Eq CapacityReservationId
derive newtype instance Ord CapacityReservationId
derive newtype instance ReadForeign CapacityReservationId
derive newtype instance WriteForeign CapacityReservationId
derive newtype instance WriteForeignKey CapacityReservationId
derive instance Newtype CapacityReservationId _
derive instance Generic CapacityReservationId _
instance Show CapacityReservationId where
  show = genericShow

newtype HpcIslandId = HpcIslandId String

derive newtype instance Eq HpcIslandId
derive newtype instance Ord HpcIslandId
derive newtype instance ReadForeign HpcIslandId
derive newtype instance WriteForeign HpcIslandId
derive newtype instance WriteForeignKey HpcIslandId
derive instance Newtype HpcIslandId _
derive instance Generic HpcIslandId _
instance Show HpcIslandId where
  show = genericShow

newtype NetworkBlockId = NetworkBlockId String

derive newtype instance Eq NetworkBlockId
derive newtype instance Ord NetworkBlockId
derive newtype instance ReadForeign NetworkBlockId
derive newtype instance WriteForeign NetworkBlockId
derive newtype instance WriteForeignKey NetworkBlockId
derive instance Newtype NetworkBlockId _
derive instance Generic NetworkBlockId _
instance Show NetworkBlockId where
  show = genericShow

newtype FaultDomainId = FaultDomainId String

derive newtype instance Eq FaultDomainId
derive newtype instance Ord FaultDomainId
derive newtype instance ReadForeign FaultDomainId
derive newtype instance WriteForeign FaultDomainId
derive newtype instance WriteForeignKey FaultDomainId
derive instance Newtype FaultDomainId _
derive instance Generic FaultDomainId _
instance Show FaultDomainId where
  show = genericShow

newtype ComputeClusterId = ComputeClusterId String

derive newtype instance Eq ComputeClusterId
derive newtype instance Ord ComputeClusterId
derive newtype instance ReadForeign ComputeClusterId
derive newtype instance WriteForeign ComputeClusterId
derive newtype instance WriteForeignKey ComputeClusterId
derive instance Newtype ComputeClusterId _
derive instance Generic ComputeClusterId _
instance Show ComputeClusterId where
  show = genericShow

newtype DedicatedVmHostId = DedicatedVmHostId String

derive newtype instance Eq DedicatedVmHostId
derive newtype instance Ord DedicatedVmHostId
derive newtype instance ReadForeign DedicatedVmHostId
derive newtype instance WriteForeign DedicatedVmHostId
derive newtype instance WriteForeignKey DedicatedVmHostId
derive instance Newtype DedicatedVmHostId _
derive instance Generic DedicatedVmHostId _
instance Show DedicatedVmHostId where
  show = genericShow

newtype InstanceId = InstanceId String

derive newtype instance Eq InstanceId
derive newtype instance Ord InstanceId
derive newtype instance ReadForeign InstanceId
derive newtype instance WriteForeign InstanceId
derive newtype instance WriteForeignKey InstanceId
derive instance Newtype InstanceId _
derive instance Generic InstanceId _
instance Show InstanceId where
  show = genericShow

newtype KmsKeyId = KmsKeyId String

derive newtype instance Eq KmsKeyId
derive newtype instance Ord KmsKeyId
derive newtype instance ReadForeign KmsKeyId
derive newtype instance WriteForeign KmsKeyId
derive newtype instance WriteForeignKey KmsKeyId
derive instance Newtype KmsKeyId _
derive instance Generic KmsKeyId _
instance Show KmsKeyId where
  show = genericShow