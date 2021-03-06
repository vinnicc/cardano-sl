-- | Utilities for block logic testing.

module Test.Pos.Block.Logic.Util
       ( EnableTxPayload (..)
       , InplaceDB (..)
       , bpGenBlocks
       , bpGenBlock
       , bpGoToArbitraryState
       , withCurrentSlot
       , satisfySlotCheck
       , getAllSecrets
       ) where

import           Universum
import           Unsafe                      (unsafeHead)

import           Control.Monad.Random.Strict (evalRandT)
import           Data.Default                (def)
import           Test.QuickCheck.Gen         (Gen (MkGen), sized)
import           Test.QuickCheck.Monadic     (pick)

import           Pos.AllSecrets              (AllSecrets, allSecrets)
import           Pos.Block.Core              (Block)
import           Pos.Block.Types             (Blund)
import           Pos.Core                    (BlockCount, GenesisData (..),
                                              HasConfiguration, SlotId (..), epochIndexL,
                                              genesisData)
import           Pos.Generator.Block         (BlockGenParams (..), genBlocks,
                                              tgpTxCountRange)
import           Pos.Launcher                (HasConfigurations)
import           Pos.Ssc.GodTossing          (SscGodTossing)
import           Pos.Util.Chrono             (NE, OldestFirst (..))
import           Pos.Util.Util               (_neLast)

import           Test.Pos.Block.Logic.Mode   (BlockProperty, BlockTestContext,
                                              btcSlotId_L)

-- | Wrapper for 'bpGenBlocks' to clarify the meaning of the argument.
newtype EnableTxPayload = EnableTxPayload Bool

-- | Wrapper for 'bpGenBlocks' to clarify the meaning of the argument.
newtype InplaceDB = InplaceDB Bool

-- | Generate arbitrary valid blocks inside 'BlockProperty'. The first
-- argument specifies how many blocks should be generated. If it's
-- 'Nothing', the number of blocks will be generated by QuickCheck
-- engine.
bpGenBlocks
    :: HasConfigurations
    => Maybe BlockCount
    -> EnableTxPayload
    -> InplaceDB
    -> BlockProperty (OldestFirst [] (Blund SscGodTossing))
bpGenBlocks blkCnt (EnableTxPayload enableTxPayload) (InplaceDB inplaceDB) = do
    allSecrets_ <- getAllSecrets
    let genStakeholders = gdBootStakeholders genesisData
    let genBlockGenParams s =
            pure
                BlockGenParams
                { _bgpSecrets = allSecrets_
                , _bgpBlockCount = fromMaybe (fromIntegral s) blkCnt
                , _bgpTxGenParams =
                      def & tgpTxCountRange %~ bool (const (0,0)) identity enableTxPayload
                , _bgpInplaceDB = inplaceDB
                , _bgpGenStakeholders = genStakeholders
                , _bgpSkipNoKey = False
                }
    params <- pick $ sized genBlockGenParams
    g <- pick $ MkGen $ \qc _ -> qc
    lift $ evalRandT (genBlocks params) g

-- | A version of 'bpGenBlocks' which generates exactly one
-- block. Allows one to avoid unsafe functions sometimes.
bpGenBlock ::
       HasConfigurations => EnableTxPayload -> InplaceDB -> BlockProperty (Blund SscGodTossing)
-- 'unsafeHead' is safe because we create exactly 1 block
bpGenBlock = fmap (unsafeHead . toList) ... bpGenBlocks (Just 1)

getAllSecrets :: BlockProperty AllSecrets
getAllSecrets = lift $ view allSecrets

-- | Go to arbitrary global state in 'BlockProperty'.
bpGoToArbitraryState :: BlockProperty ()
-- TODO: generate arbitrary blocks, apply them.
bpGoToArbitraryState = pass

-- | Perform action pretending current slot is the given one.
withCurrentSlot :: MonadReader BlockTestContext m => SlotId -> m a -> m a
withCurrentSlot slot = local (set btcSlotId_L $ Just slot)

-- | This simple helper is useful when one needs to verify
-- blocks. Blocks verification checks that blocks are not from
-- future. This function pretends that current slot is after the last
-- slot of the given blocks.
satisfySlotCheck
    :: (HasConfiguration, MonadReader BlockTestContext m)
    => OldestFirst NE (Block SscGodTossing)
    -> m a
    -> m a
satisfySlotCheck (OldestFirst blocks) action =
    let lastEpoch = blocks ^. _neLast . epochIndexL
    in withCurrentSlot (SlotId (lastEpoch + 1) minBound) action
