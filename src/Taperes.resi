module Assert: {
  type t
  let assertTrue: bool => t
}

let test: (string, unit => Assert.t) => unit
