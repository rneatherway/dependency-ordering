namespace DependencyOrdering

open Microsoft.Build
open Microsoft.Build.Framework
open Microsoft.Build.Tasks
open Microsoft.Build.Utilities
open System.Reflection
open System

type String = string
type Seq<'a> = seq<'a>
type Bool = bool

module List =
  let toStringWithDelims (fr: String) (sep: String) (bk: String) (xs: List<'a>) : String =
    let rec toSWD acc ys =
      match ys with
      | []       -> acc
      | [z]      -> sprintf "%s%A" acc z
      | y::z::zs -> toSWD (sprintf "%s%A%s" acc y sep) (z::zs)
    fr + toSWD "" xs + bk

[<CustomEquality>]
[<CustomComparison>]
[<StructuredFormatDisplay("{show}")>]
type AssemblyRef =
  {
    Path: String
    Assembly: Assembly
    Name: String
  }

  member this.show = this.ToString ()

  override this.Equals (obj: Object) : bool =
    Object.eqHack (fun (a:AssemblyRef) -> a.Name) this obj

  override this.GetHashCode () = 
    hash this.Name
  
  interface System.IComparable with 
    member this.CompareTo (obj: Object) =
      Object.compHack (fun (p:AssemblyRef) -> p.Name) this obj

  override x.ToString () = x.Path

[<Serializable>]
type ForeignAidWorker () =

  let mkGraph (seeds: seq<AssemblyRef>) : Digraph<AssemblyRef> =

    let findRef (s: Seq<AssemblyRef>) (m: AssemblyName) : Seq<AssemblyRef> =
      match Seq.tryFind (fun (r: AssemblyRef) -> r.Name = m.Name) seeds with
      | None    -> s
      | Some ar -> Seq.append (Seq.singleton ar) s

    let processNode (g: Digraph<AssemblyRef>) (n: AssemblyRef) : Digraph<AssemblyRef> =
      let depNames = n.Assembly.GetReferencedAssemblies ()
      let depRefs = Array.fold findRef Seq.empty depNames
      Seq.fold (fun h c -> Digraph.addEdge (n, c) h) g depRefs

    let rec fixpoint (g: Digraph<AssemblyRef>) : Digraph<AssemblyRef> =
      let ns = Digraph.nodes g
      let g' = List.fold processNode g ns
      if g = g' then g else fixpoint g'

    fixpoint (Seq.fold (fun g s -> Digraph.addNode s g) Map.empty seeds)

  let mkAssemblyRef (t: String) : AssemblyRef =
    let asmBytes = System.IO.File.ReadAllBytes(t)
    let assm = Assembly.Load(asmBytes)
    {
      Path = t
      Assembly = assm
      Name = assm.GetName().Name
    }

  member x.Work(rs: String[]) : String = 
    let asmRefs = Array.map mkAssemblyRef rs
    let graph = mkGraph asmRefs
    let ordering = Digraph.topSort graph
    let str = List.toStringWithDelims "#r @\"" "\"\n#r @\"" "\"" ordering
    str 

type OrderAssemblyReferences() = 
  inherit Task ()

  member val ReferencePaths = null with get,set

  [<Output>]
  member val Ordering = "" with get,set

  override x.Execute () : Bool =
    let setup = AppDomainSetup()
    do setup.ApplicationBase <- System.IO.Path.GetDirectoryName(System.Reflection.Assembly.GetExecutingAssembly().Location)
    let appDomain = AppDomain.CreateDomain("TestDomain", null, setup)
    try 
      let paths = Array.map (fun (i: ITaskItem) -> i.ItemSpec) x.ReferencePaths
      let faw = (appDomain.CreateInstanceAndUnwrap(typeof<ForeignAidWorker>.Assembly.FullName, typeof<ForeignAidWorker>.FullName)) :?> ForeignAidWorker
      let ordering = faw.Work(paths)
      do x.Ordering <- ordering
      true
    finally
      do AppDomain.Unload(appDomain)
    
