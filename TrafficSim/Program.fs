// Learn more about F# at http://fsharp.org

open System
open Microsoft.FSharp.Data.UnitSystems.SI.UnitNames
open System.Linq.Expressions
open System.Xml

[<Measure>] type millimetre
let mmPerMetre = 1000<millimetre/metre>
let g = 9.8<metre/second^2>

type Road = {
    Pavement : int
    Covering : int
}

type VehicleModel = {
    width : float<metre>
    length : float<metre>
    mass : float<kilogram>
    turningRadius: float<metre>
    traction : bool -> Road -> float 
    drag : float<newton>
    maxPower : float<watt>
    idlePower : float<watt>
}

type Vehicle = {
    model : VehicleModel
    x : float<metre>
    y : float<metre>
    vx : float<metre/second>
    vy : float<metre/second>
    power : float<watt>
    steering : float<1/metre>
    braking: float<newton>
    heading : float
}

let magnitude (parts : List<float<_>>) = 
    parts |> List.map (fun x -> x * x) |> List.sum |> sqrt

let maxTraction vehicle road = 
    vehicle.model.traction true road * g 

let maxForward vehicle : float<metre/second^2>= 
    (vehicle.model.maxPower / sqrt (vehicle.vx * vehicle.vx + vehicle.vy * vehicle.vy) - vehicle.model.drag) / vehicle.model.mass

let calculateAcceleration vehicle road = 
    let speed : float<metre/second> = magnitude [vehicle.vx; vehicle.vy]
    let motorforce : float<newton> = vehicle.power / speed 
    let forward : float<metre/second^2>= (motorforce + (if motorforce > 0.<newton> then -vehicle.model.drag else vehicle.model.drag) - vehicle.braking) / vehicle.model.mass
    let transverse : float<metre/second^2> = speed * speed * vehicle.steering
    let ax : float<metre/second^2> = cos vehicle.heading * forward + - sin vehicle.heading * transverse
    let ay : float<metre/second^2> = sin(vehicle.heading) * forward + cos vehicle.heading * transverse
    let am : float<metre/second^2> = magnitude [ax; ay]
    let amax : float<metre/second^2> = maxTraction vehicle road
    if am > amax then (ax * amax / am, ay * amax / am) else (ax, ay)

let simulateVehicle control vehicle road time = 
    let power, steering, braking = control vehicle road
    let ax, ay = calculateAcceleration vehicle road
    let (|Positive|Negative|Zero|) n = if n > 0.<_> then Positive elif n < 0.<_> then Negative else Zero
    {
        model = vehicle.model
        x = vehicle.x + vehicle.vx * time
        y = vehicle.y + vehicle.vy * time
        vx = vehicle.vx + ax * time
        vy = vehicle.vy + ay * time
        power = power
        steering = steering
        braking = braking
        heading = match vehicle.vx with
            | Zero -> match vehicle.vy with 
                |Zero -> vehicle.heading
                |Positive -> Math.PI/2.
                |Negative -> -Math.PI/2.
            | Positive -> atan2 vehicle.vy vehicle.vx
            | Negative -> Math.PI + atan2 vehicle.vy vehicle.vx
                 
    }
let rec next prev n =
    if n = 0 then prev
    else
        let r = simulateVehicle (fun vehicle _ -> (vehicle.model.idlePower, 0.</metre>, 0.<newton>)) prev {Pavement=1; Covering=1} 1.<second>
        printfn "%A" r
        next r (n-1)
[<EntryPoint>]
let main argv =
    let m = { 
        width = 2.<metre>
        length = 5.<metre>
        mass = 2000.<kilogram>
        turningRadius = 5.<metre>
        traction = (fun a b -> 0.9)
        drag = 1000.<newton>
        maxPower = 70000.<watt>
        idlePower = 2000.<watt>
    }
    let v = {
        model = m
        x = 0.<metre>
        y = 0.<metre>
        vx = 10.<metre/second>
        vy = 0.<metre/second>
        power = 0.<watt>
        braking = 0.<newton>
        steering = 0.</metre>
        heading = 0.
    }

    next v 100 |> ignore

    Console.In.ReadLine () |> ignore

    0 // return an integer exit code
