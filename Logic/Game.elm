module Logic.Game exposing (checkBackLog)
import Types exposing (..)
import Constants exposing (..)
import Array exposing (..)


-- Returns True for "game over".
-- Happens with more than "maxLanes" over "maxBackLog"
checkBackLog: Int -> Int -> Array Lane -> Bool
checkBackLog maxBackLog maxLanes lanes  =
  let
    lanesOverLimit = lanes |> Array.filter (\lane -> lane.carBacklog > maxBackLog) |> Array.length
  in
    lanesOverLimit > maxLanes
