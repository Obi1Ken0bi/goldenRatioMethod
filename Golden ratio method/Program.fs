let phi = 0.5 * (1.0 + sqrt 5.0)
let i=ref 0
let minimize f eps a b = 
    let rec min_rec f eps a b fx1 fx2 =
        i.Value |> printfn "%d"
        i.Value<-i.Value+1
        if b - a < eps then 
            0.5 * (a + b)
        else 
            let t = (b - a) / phi
            let x1, x2 = b - t, a + t
            let fx1 = match fx1 with Some v -> v | None -> f x1
            let fx2 = match fx2 with Some v -> v | None -> f x2
            
            if fx1 >= fx2 then
                min_rec f eps x1 b (Some fx2) None
            else
                min_rec f eps a x2 None (Some fx1)
    min_rec f eps (min a b) (max a b) None None

minimize (fun x -> ((10.0*(x**3.0)+3.0*(x**2.0)+x+5.0)**2.0)) 1e-12 -2.0 2.0 |> printfn "%.10g"
