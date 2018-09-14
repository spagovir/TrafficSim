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
    vehicle.model.traction vehicle.hasTraction road * g 

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
    let (|Negative|Positive|Zero|) (num : float<_>)= if num < -0.1<_> then Negative elif num > 0.1<_> then Positive else Zero
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
                | Zero -> vehicle.heading
                | Positive -> Math.PI / 2.0
                | Negative -> -Math.PI / 2.0
            | Positive -> atan2 vehicle.vy vehicle.vx
            | Negative -> atan2 vehicle.vy vehicle.vx - Math.PI
    }



[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    0 // return an integer exit code
