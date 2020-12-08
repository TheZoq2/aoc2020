module Processor where

import Data.Maybe as Maybe
import Clash.Prelude
import Clash.Annotations.SynthesisAttributes
-- import Debug.Trace

data Instruction
    = Nop (Signed 16)
    | Acc (Signed 16)
    | Jmp (Signed 16)
    deriving (Show, Generic, NFDataX)


data State = State
    { pc :: Signed 16
    , acc :: Signed 16
    , output :: Maybe (Signed 16)
    , visited :: BitVector 650
    } deriving (Show, Generic, NFDataX)


transitionFunction :: State -> Instruction -> (State, Maybe (Signed 16))
transitionFunction state ins =
    let
        idx = fromInteger (toInteger $ pc state)
        prevBit = testBit (visited state) idx
        newVisited = setBit (visited state) idx

        newOutput =
            if prevBit then
                if Maybe.isNothing (output state) then
                    Just $ acc state
                else
                    output state
            else
                Nothing
        state' = case ins of
            Nop _ -> state { pc = pc state + 1 }
            Acc val -> state { acc = acc state + val, pc = pc state + 1 }
            Jmp val ->
                let
                    newPc = pc state + val
                 in state { pc = newPc, visited = newVisited, output = newOutput }
    in (state' {visited = newVisited, output = newOutput}, newOutput)


processor :: KnownNat n => Vec n Instruction -> State -> () -> (State, Maybe (Signed 16))
processor program state _input =
    let
        ins = program !! (pc state)
    in transitionFunction state ins


testProgram =
    ( Jmp 336
    :> Jmp 593
    :> Jmp 121
    :> Acc (-8)
    :> Nop 459
    :> Jmp 451
    :> Acc (-6)
    :> Acc 23
    :> Acc 23
    :> Acc (-2)
    :> Jmp 113
    :> Acc (-11)
    :> Acc 25
    :> Jmp 529
    :> Acc 0
    :> Jmp 1
    :> Jmp 313
    :> Acc 30
    :> Nop 235
    :> Jmp 45
    :> Nop 195
    :> Acc (-11)
    :> Jmp 491
    :> Acc 6
    :> Nop 425
    :> Nop 68
    :> Acc 9
    :> Jmp (-25)
    :> Jmp 507
    :> Jmp 456
    :> Acc (-1)
    :> Acc 49
    :> Acc 5
    :> Jmp 31
    :> Acc 30
    :> Nop 513
    :> Jmp 499
    :> Nop 533
    :> Acc 18
    :> Acc 28
    :> Acc (-2)
    :> Jmp 170
    :> Acc (-5)
    :> Jmp 180
    :> Nop 179
    :> Acc 15
    :> Acc 33
    :> Nop 27
    :> Jmp 424
    :> Acc 30
    :> Acc 33
    :> Acc 50
    :> Jmp 348
    :> Jmp 435
    :> Acc 21
    :> Acc 14
    :> Acc 13
    :> Jmp 1
    :> Jmp 98
    :> Jmp 39
    :> Acc 11
    :> Acc 23
    :> Acc 44
    :> Jmp 494
    :> Acc 17
    :> Acc (-4)
    :> Acc 24
    :> Acc 5
    :> Jmp 204
    :> Nop 454
    :> Acc 45
    :> Acc (-10)
    :> Acc (-3)
    :> Jmp 78
    :> Acc 47
    :> Acc 49
    :> Nop 99
    :> Acc 29
    :> Jmp (-76)
    :> Acc (-9)
    :> Acc 43
    :> Jmp 279
    :> Jmp 460
    :> Jmp 246
    :> Jmp (-78)
    :> Acc (-8)
    :> Acc (-3)
    :> Jmp 415
    :> Acc (-3)
    :> Acc (-12)
    :> Jmp 117
    :> Acc 32
    :> Jmp 206
    :> Acc 6
    :> Acc 19
    :> Jmp 291
    :> Acc 8
    :> Jmp 175
    :> Acc 2
    :> Jmp (-6)
    :> Acc 11
    :> Acc 28
    :> Acc (-19)
    :> Acc 1
    :> Jmp 194
    :> Acc 29
    :> Nop 387
    :> Acc 21
    :> Jmp 507
    :> Jmp 304
    :> Acc 22
    :> Acc (-14)
    :> Acc (-6)
    :> Acc 45
    :> Jmp 101
    :> Acc 12
    :> Jmp 91
    :> Acc 47
    :> Jmp 171
    :> Acc (-4)
    :> Acc 24
    :> Acc 3
    :> Jmp 29
    :> Nop 311
    :> Acc (-12)
    :> Jmp 179
    :> Jmp 1
    :> Acc (-3)
    :> Acc 6
    :> Acc 22
    :> Jmp 214
    :> Nop (-101)
    :> Jmp (-118)
    :> Acc 23
    :> Acc 15
    :> Jmp (-42)
    :> Acc 22
    :> Nop 391
    :> Jmp 27
    :> Acc (-9)
    :> Jmp 449
    :> Jmp 453
    :> Jmp 208
    :> Jmp 480
    :> Jmp 335
    :> Acc 23
    :> Jmp 1
    :> Acc 3
    :> Acc 45
    :> Jmp (-18)
    :> Jmp 335
    :> Jmp 165
    :> Acc 26
    :> Acc 8
    :> Acc (-3)
    :> Jmp (-57)
    :> Jmp (-112)
    :> Acc 14
    :> Acc 20
    :> Acc 21
    :> Acc 13
    :> Jmp (-20)
    :> Acc 35
    :> Jmp (-81)
    :> Jmp 406
    :> Acc (-16)
    :> Acc 30
    :> Acc (-18)
    :> Jmp 79
    :> Jmp 103
    :> Jmp (-96)
    :> Acc 43
    :> Acc 36
    :> Acc 31
    :> Jmp 423
    :> Nop 13
    :> Acc (-11)
    :> Acc 46
    :> Nop 208
    :> Jmp 398
    :> Acc 50
    :> Jmp 326
    :> Acc 31
    :> Jmp 45
    :> Acc 50
    :> Acc 13
    :> Acc 38
    :> Jmp 60
    :> Nop (-37)
    :> Nop (-79)
    :> Jmp 213
    :> Acc (-19)
    :> Acc (-16)
    :> Jmp 30
    :> Jmp 1
    :> Acc 27
    :> Acc (-4)
    :> Acc 46
    :> Jmp (-81)
    :> Jmp 29
    :> Nop 23
    :> Nop 388
    :> Acc 40
    :> Acc 46
    :> Jmp 19
    :> Acc 17
    :> Jmp 287
    :> Jmp 265
    :> Acc 17
    :> Acc 49
    :> Jmp 353
    :> Acc (-4)
    :> Acc 39
    :> Jmp 397
    :> Jmp 84
    :> Jmp (-156)
    :> Jmp (-41)
    :> Acc 2
    :> Acc 7
    :> Acc 45
    :> Acc 26
    :> Jmp 376
    :> Jmp 143
    :> Jmp (-195)
    :> Acc 1
    :> Acc (-16)
    :> Acc 47
    :> Jmp (-225)
    :> Acc (-11)
    :> Acc (-7)
    :> Nop (-166)
    :> Jmp 297
    :> Acc 12
    :> Acc 14
    :> Acc (-17)
    :> Jmp 9
    :> Acc 49
    :> Acc (-9)
    :> Nop (-31)
    :> Acc 17
    :> Jmp (-130)
    :> Acc 3
    :> Acc 41
    :> Jmp 112
    :> Acc (-11)
    :> Jmp 21
    :> Jmp 125
    :> Acc 2
    :> Acc 37
    :> Jmp (-165)
    :> Acc 9
    :> Acc 21
    :> Jmp (-35)
    :> Nop 9
    :> Nop 303
    :> Acc (-6)
    :> Acc 12
    :> Jmp 177
    :> Acc 36
    :> Acc 15
    :> Nop (-207)
    :> Jmp 224
    :> Jmp (-106)
    :> Acc 18
    :> Nop (-166)
    :> Jmp 39
    :> Jmp 175
    :> Acc (-11)
    :> Acc (-13)
    :> Acc 23
    :> Acc (-6)
    :> Jmp (-269)
    :> Acc 8
    :> Nop 212
    :> Jmp (-123)
    :> Jmp 188
    :> Acc 35
    :> Acc 43
    :> Acc 33
    :> Jmp (-91)
    :> Acc 39
    :> Acc 8
    :> Acc (-1)
    :> Acc 15
    :> Jmp 29
    :> Acc 44
    :> Nop 18
    :> Jmp (-250)
    :> Jmp 83
    :> Acc 10
    :> Acc 12
    :> Acc 18
    :> Jmp 1
    :> Jmp 170
    :> Acc 47
    :> Acc (-13)
    :> Acc 4
    :> Jmp 32
    :> Acc 0
    :> Jmp 203
    :> Acc (-16)
    :> Acc 11
    :> Acc 38
    :> Jmp (-103)
    :> Jmp 2
    :> Jmp (-69)
    :> Nop (-292)
    :> Jmp (-90)
    :> Nop (-97)
    :> Acc 29
    :> Nop 124
    :> Acc (-12)
    :> Jmp (-238)
    :> Acc 4
    :> Jmp 36
    :> Jmp (-108)
    :> Acc 14
    :> Acc 21
    :> Jmp 82
    :> Acc (-6)
    :> Acc 28
    :> Jmp (-133)
    :> Acc (-4)
    :> Acc 28
    :> Jmp 155
    :> Nop 168
    :> Acc (-2)
    :> Nop 86
    :> Jmp 287
    :> Acc 15
    :> Acc 46
    :> Acc 40
    :> Acc (-16)
    :> Jmp (-139)
    :> Acc 39
    :> Jmp 36
    :> Acc (-16)
    :> Acc 25
    :> Nop (-215)
    :> Jmp 146
    :> Nop (-296)
    :> Acc (-12)
    :> Acc 40
    :> Jmp (-299)
    :> Acc 48
    :> Acc 37
    :> Nop (-205)
    :> Acc (-17)
    :> Jmp 215
    :> Jmp 254
    :> Jmp 53
    :> Acc 15
    :> Acc 34
    :> Acc 8
    :> Jmp 216
    :> Nop 5
    :> Acc 23
    :> Jmp (-195)
    :> Acc 0
    :> Jmp 69
    :> Acc 6
    :> Jmp 187
    :> Acc 14
    :> Acc (-2)
    :> Jmp (-51)
    :> Acc (-12)
    :> Acc 43
    :> Nop (-51)
    :> Jmp (-193)
    :> Acc (-1)
    :> Jmp (-234)
    :> Acc 20
    :> Jmp 1
    :> Acc 33
    :> Acc 23
    :> Jmp (-131)
    :> Acc 38
    :> Acc 2
    :> Jmp 179
    :> Jmp 1
    :> Jmp 209
    :> Nop (-187)
    :> Acc 41
    :> Acc 32
    :> Acc 6
    :> Jmp 209
    :> Nop (-230)
    :> Acc 37
    :> Acc 47
    :> Acc 32
    :> Jmp 82
    :> Acc 45
    :> Acc 21
    :> Jmp (-108)
    :> Acc 13
    :> Acc 39
    :> Jmp 127
    :> Jmp 1
    :> Nop 196
    :> Jmp (-146)
    :> Acc 19
    :> Jmp (-125)
    :> Jmp (-149)
    :> Acc 28
    :> Acc (-1)
    :> Acc 35
    :> Acc 7
    :> Jmp (-262)
    :> Acc (-16)
    :> Acc (-3)
    :> Acc 48
    :> Acc 13
    :> Jmp 177
    :> Jmp (-91)
    :> Acc 26
    :> Nop 78
    :> Acc 15
    :> Jmp 1
    :> Jmp 197
    :> Acc 12
    :> Acc (-14)
    :> Acc 9
    :> Jmp (-317)
    :> Acc 41
    :> Acc 2
    :> Acc (-11)
    :> Acc 6
    :> Jmp (-284)
    :> Acc 28
    :> Acc (-16)
    :> Jmp (-65)
    :> Acc 31
    :> Nop (-276)
    :> Jmp (-205)
    :> Acc 6
    :> Acc 33
    :> Acc 7
    :> Acc (-2)
    :> Jmp (-125)
    :> Acc 34
    :> Jmp (-193)
    :> Acc (-12)
    :> Acc 39
    :> Jmp 1
    :> Jmp (-208)
    :> Acc 3
    :> Acc 0
    :> Nop 9
    :> Jmp (-112)
    :> Jmp 1
    :> Jmp (-314)
    :> Acc 34
    :> Acc 9
    :> Acc 5
    :> Acc 20
    :> Jmp 139
    :> Acc (-8)
    :> Acc 11
    :> Acc 18
    :> Jmp 1
    :> Jmp (-66)
    :> Acc (-14)
    :> Jmp (-173)
    :> Acc (-3)
    :> Acc 31
    :> Acc 0
    :> Jmp 117
    :> Acc 47
    :> Acc 24
    :> Acc 9
    :> Acc 43
    :> Jmp (-427)
    :> Acc (-11)
    :> Nop (-226)
    :> Jmp (-444)
    :> Acc 43
    :> Acc (-6)
    :> Acc (-3)
    :> Jmp (-202)
    :> Acc 34
    :> Jmp (-337)
    :> Acc 36
    :> Acc 8
    :> Acc 9
    :> Jmp (-162)
    :> Jmp 118
    :> Acc 18
    :> Jmp (-481)
    :> Nop (-144)
    :> Nop (-236)
    :> Jmp (-290)
    :> Acc (-14)
    :> Jmp (-448)
    :> Acc 29
    :> Acc (-19)
    :> Acc 21
    :> Acc (-11)
    :> Jmp (-78)
    :> Acc 1
    :> Acc 30
    :> Acc (-2)
    :> Jmp 114
    :> Jmp 11
    :> Jmp (-117)
    :> Jmp (-322)
    :> Acc 23
    :> Jmp (-361)
    :> Jmp (-337)
    :> Acc 40
    :> Nop 85
    :> Nop (-206)
    :> Acc 12
    :> Jmp (-199)
    :> Acc 38
    :> Acc 20
    :> Acc 19
    :> Acc 33
    :> Jmp (-61)
    :> Acc 30
    :> Nop (-234)
    :> Acc 6
    :> Acc 24
    :> Jmp (-463)
    :> Acc (-10)
    :> Jmp 79
    :> Acc 43
    :> Acc 45
    :> Jmp (-261)
    :> Acc (-13)
    :> Acc 1
    :> Nop (-217)
    :> Nop (-82)
    :> Jmp (-153)
    :> Acc 39
    :> Jmp (-340)
    :> Acc 37
    :> Acc 17
    :> Jmp (-182)
    :> Acc 26
    :> Acc 1
    :> Acc 23
    :> Jmp 16
    :> Acc 39
    :> Acc 13
    :> Jmp (-40)
    :> Acc 37
    :> Acc (-7)
    :> Jmp (-119)
    :> Acc 20
    :> Acc 44
    :> Acc 44
    :> Acc 14
    :> Jmp (-464)
    :> Acc 6
    :> Acc 14
    :> Acc 7
    :> Jmp 54
    :> Acc 10
    :> Jmp (-538)
    :> Acc 13
    :> Acc (-1)
    :> Acc 22
    :> Jmp (-397)
    :> Acc 17
    :> Acc (-8)
    :> Acc (-5)
    :> Jmp (-230)
    :> Acc 38
    :> Nop (-218)
    :> Jmp (-81)
    :> Acc (-19)
    :> Nop (-14)
    :> Acc 32
    :> Jmp (-311)
    :> Nop (-32)
    :> Acc 18
    :> Jmp (-232)
    :> Acc 40
    :> Nop (-242)
    :> Acc (-11)
    :> Acc 34
    :> Jmp (-528)
    :> Jmp (-484)
    :> Jmp (-155)
    :> Acc (-2)
    :> Acc 50
    :> Acc (-11)
    :> Acc 50
    :> Jmp (-116)
    :> Nop (-361)
    :> Acc 12
    :> Jmp (-293)
    :> Acc 39
    :> Jmp 17
    :> Jmp (-37)
    :> Acc (-14)
    :> Jmp (-440)
    :> Jmp (-226)
    :> Acc 16
    :> Acc 25
    :> Acc 50
    :> Acc 49
    :> Jmp (-242)
    :> Acc 47
    :> Acc (-19)
    :> Nop (-435)
    :> Acc 41
    :> Jmp (-523)
    :> Acc 6
    :> Jmp (-458)
    :> Acc 0
    :> Acc 37
    :> Jmp (-528)
    :> Acc 27
    :> Jmp (-57)
    :> Acc 47
    :> Acc (-9)
    :> Acc 23
    :> Jmp (-205)
    :> Acc 40
    :> Nop (-207)
    :> Acc (-6)
    :> Jmp (-129)
    :> Jmp 1
    :> Acc 24
    :> Acc (-14)
    :> Jmp 1
    :> Jmp 1
    :> Nil
    )


runTestProgram :: State -> (Signed 16)
runTestProgram State {output = Just val} = val
runTestProgram state =
    let
        insn = testProgram !! (pc state)

        (nstate, _) = transitionFunction state insn
    in
    runTestProgram nstate

initialState :: State
initialState =
    State
        { pc = 0
        , acc = 0
        , output = Nothing
        , visited = 0
        }




-- Main module which receives signals from RAM, from the model and additional payload
-- to be delayed together with the pipeline.
main :: HiddenClockResetEnable dom =>
    Signal dom () -> Signal dom (Maybe (Signed 16))
main input =
    mealy (processor testProgram) initialState input



{-# ANN topEntity
  (Synthesize
    { t_name   = "processor"
    , t_inputs = [ PortName "clk"
                 , PortName "rst"
                 , PortName "enable"
                 ]
    , t_output = PortProduct ""
        [ PortName "output_valid"
        , PortName "value"
        ]
    }) #-}
topEntity
  :: Clock System
  -> Reset System
  -> Enable System
  -> Signal System ()
  -> Signal System (Maybe (Signed 16))
topEntity = exposeClockResetEnable main
