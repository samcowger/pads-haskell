{-# LANGUAGE TypeFamilies
           , ScopedTypeVariables
           , DeriveDataTypeable
           , MultiParamTypeClasses
           , TypeSynonymInstances
           , TemplateHaskell
           , QuasiQuotes
           , FlexibleInstances
           , FlexibleContexts
           , RecordWildCards
           , NamedFieldPuns #-}



module Language.Pads.DataGen.GenDescriptions where

import Language.Pads.Padsc

padsSamples = [pads|

    data Simple = Simple {
        c1 :: Char,
        '|',
        "constant string",
        '|',
        i1 :: Int,
        '|',
        r1 :: StringFW 20
    }

    data Name = Bob "Bob" | Fred "Fred"

    data Record = Record {
        "first:",
        fname :: Name, ' ',
        "middle:",
        mname :: Name, ' ',
        "last:",
        lname :: Name, ' ',
        "age:",
        age :: Int, ' ',
        "height:",
        height :: Int
    }

    data Records = Records [Record | " | "]

    newtype Addresses = Addresses [Address]
    data Address = Address {x :: Int, ' ', y :: StringC '\n'}

    -- START   -- _ --> S1
    -- S1      -- c --> S2
    -- S2      -- a --> S3
    -- S3      -- a --> S3
    -- S3      -- t --> ACCEPT

    data START = START { S1 }

    data S1 = S1 { 'c', S2 }

    data S2 = S2 { 'a', S3 }

    data S3 = S3A { 'a', S3 }
            | S3T { 't', S4 }

    data S4 = S4 { }

--                                     ___ a ___
--                                   /           \
--  |                               |             |      |--------|
--  |    |----|          |----|      \-> |----| -/       | |----| |
--   \-> | S1 | -- c --> | S2 | -- a --> | S3 | -- t --> | | S4 | |
--       |----|          |----|          |----|          | |----| |
--                                                       |--------|

    data Pixel = Pixel {
        a :: Bits16 9,
        b :: Bits8 5,
        c :: Bits8 5,
        d :: Bits8 5,
        pb_index :: Bits8 4,
        pr_index :: Bits8 4
    }

    data Mixed = Mixed {
        bits1 :: Bits8 4,
        'c',
        bits2 :: Bits8 4
    }

    data DT = DT1 [Int | ',']
            | DT2 " START DT2 " DT "||" DT " END DT2 "

|]




-- testdfa (mk_render_char ast_START)
