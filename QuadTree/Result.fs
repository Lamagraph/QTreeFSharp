module Result

type Result<'success, 'failure> =
    | Success of 'success
    | Failure of 'failure
