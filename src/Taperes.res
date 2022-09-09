@val external process: {..} = "process"

let nextTick: (unit => unit) => unit = process["nextTick"]

let isRunning = ref(false)

let failCounter = ref(0)
let passCounter = ref(0)

type status = Ok | NotOk

module Assert = {
  type t = status

  let assertTrue = b => b ? Ok : NotOk
}

let testQueue: MutableQueue.t<(string, unit => Assert.t)> = MutableQueue.make()

let test = (desc, testFn) => {
  if isRunning.contents {
    Js.Console.error("Test suite already running, use `test` at top level.")
    process["exit"](. -1)
  }

  MutableQueue.add(testQueue, (desc, testFn))
}

let getTestOutput = (status: status, number: int, rawDesc: string) => {
  let desc = switch rawDesc->Js.String2.trim {
  | "" => ""
  | s => " - " ++ s
  }
  let numberStr = number->Int.toString

  let output = switch status {
  | Ok => `ok ${numberStr}${desc}`
  | NotOk => `not ok ${numberStr}${desc}`
  }

  output
}

let run = () => {
  isRunning := true
  Js.log("TAP version 13")
  let testCount = MutableQueue.size(testQueue)
  Js.log(`1..${testCount->Int.toString}`)

  let i = ref(1)
  testQueue->MutableQueue.forEach(((desc, testFn)) => {
    let status = testFn()
    switch status {
    | Ok => incr(passCounter)
    | NotOk => incr(failCounter)
    }

    let testOutput = getTestOutput(status, i.contents, desc)
    Js.log(testOutput)

    incr(i)
  })

  Js.log(`# tests ${testCount->Int.toString}`)
  Js.log(`# pass ${passCounter.contents->Int.toString}`)
  if failCounter.contents > 0 {
    Js.log(`# fail ${failCounter.contents->Int.toString}`)
  }
}

process["on"](."exit", (code: int) =>
  if code != 0 {
    process["exit"](. code)
  } else if failCounter.contents > 0 {
    process["exit"](. -1)
  } else {
    process["exit"](. 0)
  }
)

let () = nextTick(run)
