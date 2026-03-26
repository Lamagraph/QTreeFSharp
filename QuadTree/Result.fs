module Result

type _Result<'success, 'failure> =
    | Success of 'success
    | Failure of 'failure
