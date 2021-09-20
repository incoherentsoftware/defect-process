module Main
    ( main
    ) where

import AppEnv
import Configs
import Game
import Window

main :: IO ()
main = do
    cfgs       <- mkConfigs
    window     <- mkWindow cfgs
    appEnvData <- mkAppEnvData window cfgs

    runGame appEnvData window
