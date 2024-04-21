// For more information see https://aka.ms/fsharp-console-apps
open TorchSharp

open type TorchSharp.torch
open type TorchSharp.torch.nn
open type TorchSharp.torch.nn.functional
open type TorchSharp.TensorExtensionMethods

type Trivial() as this = 
    inherit nn.Module<Tensor,Tensor>("Trivial")

    let lin1 = nn.Linear(1000L, 100L)
    let lin2 = nn.Linear(100L, 10L)

    // The CustomModule ‘RegisterComponents’ will automatically find all fields 
    // that are either modules or tensors, register the former as modules, and 
    // the latter as buffers. It registers all of these using the name of the field, 
    // just like the PyTorch Module base class does.
    do
        this.RegisterComponents()

    override _.forward(input) = 
        
        // use is like let but adds a call to Dispose when the value goes out of scope
        // let also works here, but tutorials and whatnot use use ¯\_(ツ)_/¯
        use x = lin1.forward(input)
        use y = nn.functional.relu(x)
        lin2.forward(y)

/////////////////////////

let dataBatch = rand(32, 1000)  // Our pretend input data
let resultBatch = rand(32,10)  // Our pretend ground truth.

let loss x y = nn.functional.mse_loss(x,y)

//let model = new Trivial()

// Easier way to build model: 
let model = nn.Sequential(
    ("lin1", nn.Linear(1000L, 100L) :> nn.Module<Tensor,Tensor>), 
    ("relu", nn.ReLU() :> nn.Module<Tensor,Tensor>), 
    ("lin2", nn.Linear(100L, 10L) :> nn.Module<Tensor,Tensor>)
)

//let word2vec_len = 1000L
//let sentence_length = 32L
//let num_classes = 10L
//let model = nn.Sequential(
//    ("conv1", nn.Conv1d(word2vec_len, 128L, 5L) :> nn.Module<Tensor,Tensor>),
//    ("relu1", nn.ReLU() :> nn.Module<Tensor,Tensor>),
//    ("pool", nn.MaxPool1d(2L, 2L) :> nn.Module<Tensor,Tensor>),
//    ("flatten", nn.Flatten() :> nn.Module<Tensor,Tensor>),
//    ("dense1", nn.Linear(128L * sentence_length / 2L, 20L) :> nn.Module<Tensor,Tensor>),
//    ("relu2", nn.ReLU() :> nn.Module<Tensor,Tensor>),
//    ("dense2", nn.Linear(20L, num_classes) :> nn.Module<Tensor,Tensor>)
//    //("softmax", nn.Softmax() :> nn.Module<Tensor,Tensor>)
//)

let learning_rate = 0.01
let optimizer = torch.optim.Adam(model.parameters())

for epoch = 1 to 500 do
    // Compute the loss
    let pred = model.forward(dataBatch)
    let output = loss pred resultBatch

    // Clear the gradients before doing the back-propagation
    model.zero_grad()

    // Do back-progatation, which computes all the gradients.
    output.backward()

    optimizer.step() |> ignore

    printfn "%A" ((loss pred resultBatch).item<single>())
