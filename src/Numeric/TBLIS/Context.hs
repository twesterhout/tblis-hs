{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Copyright: (c) 2020-2021 Tom Westerhout
-- SPDX-License-Identifier: BSD-3-Clause
-- Maintainer: Tom Westerhout <14264576+twesterhout@users.noreply.github.com>
module Numeric.TBLIS.Context
  ( -- | Provides "Language.C.Inline.Context" with type mappings specific to
    -- TBIS library.
    tblisCtx,
  )
where

import qualified Data.Map as Map
import Language.C.Inline.Context (Context (..))
import qualified Language.C.Types as Types
import qualified Language.Haskell.TH as TH
import Numeric.TBLIS.Types

tblisTypesTable :: Map.Map Types.TypeSpecifier TH.TypeQ
tblisTypesTable =
  Map.fromList
    [ (Types.TypeName "len_type", [t|TblisLenType|]),
      (Types.TypeName "stride_type", [t|TblisStrideType|]),
      (Types.TypeName "label_type", [t|TblisLabelType|])
    ]

tblisCtx :: Context
tblisCtx = mempty {ctxTypesTable = tblisTypesTable}
