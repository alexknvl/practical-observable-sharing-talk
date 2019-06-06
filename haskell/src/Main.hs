module Main where

import qualified Slides.Example1_CFG    as E1
import qualified Slides.Example2_Signal as E2
import qualified Slides.RefEq_bad       as E3
import qualified Slides.RecursiveDo     as E4
import qualified Circuits.Network       as E5
import qualified Parsing.SimpleLang     as E6
import qualified Slides.Example3_CFG    as E7
import qualified Slides.Example4_MagicGraphs    as E8

main :: IO ()
main = do
  E1.main'
  E2.main'
  E3.main'
  E4.main'
  E5.main'
  E6.main'
  E7.main'
  E8.main'