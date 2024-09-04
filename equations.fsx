let eps = 0.000001

let rec dichotomy f a b = 
  if (b - a) <= eps then  
    ((a + b) / 2.)
  else
    let c = (a + b) / 2.
    if ((f b) * (f c)) < 0. then 
      dichotomy f c b
    else
      dichotomy f a c

let rec iter f acc =
  let cur_res = f acc
  if (abs (cur_res - acc)) <= eps then 
    cur_res
  else 
    iter f cur_res

let iterations phi x0 = iter (fun acc -> (phi acc)) x0

let newthon f f' x0 = iter (fun acc -> acc - (f acc) / (f' acc)) x0

// Solve 3 equations using three methods defined above
let f1 x = x - (1. / ( 3. + (sin ( 3.6 * x)))) 
let f2 x = 0.1 * (x ** 2.) - (x * (log x)) 
let f3 x = (tan x)  - ((1. / 3.) * ((tan x) ** 3.)) + ((1. / 5.) * ((tan x) ** 5.)) - 1. / 3.

let f1' x = ( 18. * (cos ( 3.6 * x))) / ( 5. * ( ((sin (3.6 * x)) ** 2.) + (6. * (sin (3.6 * x))) + 9.))
let f2' x = ((cos x) * (sin x)) + x) / float ((cos x) * (cos x)) 
let f3' x = ((tan x) ** 4. - (tan x) ** 2. + 1.) / ((cos x) ** 2.)

let phi1 x = x - f1 x / f1' x
let phi2 x = x - f2 x / f2' x
let phi3 x = x - f3 x / f3' x

let main = 
    printfn "%10.5f  %10.5f %10.5f" (dichotomy f1 2. 3.) (iterations phi1 2.) (newthon f1 f1' 2.)
    printfn "%10.5f  %10.5f %10.5f" (dichotomy f2 0.2 1.) (iterations phi2 0.2) (newthon f2 f2' 0.2)
    printfn "%10.5f  %10.5f %10.5f" (dichotomy f3 1. 2.) (iterations phi3 1.) (newthon f3 f3' 2.)
