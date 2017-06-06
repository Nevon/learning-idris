module Vehicle

data PowerSource = Pedal | Petrol

data Vehicle : PowerSource -> Type where
    Bicycle : Vehicle Pedal
    Bus : (fuel : Nat) -> Vehicle Petrol
    Car : (fuel : Nat) -> Vehicle Petrol

wheels : Vehicle petrol -> Nat
wheels Bicycle = 2
wheels (Bus fuel) = 4
wheels (Car fuel) = 4

refuel : Vehicle Petrol -> Vehicle Petrol
refuel (Bus fuel) = Bus 200
refuel (Car fuel) = Car 100
refuel Bicycle impossible
