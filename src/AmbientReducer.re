open Ambient;

type event = {
  e: string,
  t: string,
};

let inheritChildren = (b, a) => {
  addChildren(getChildren(b), a);
};

let inheritCapabilities = (b, a) => {
  List.append(getCapabilities(a), getCapabilities(b))
  |> updateCapabilities(a);
};

let inheritSpawns = (b, a) => {
  List.append(getSpawns(a), getSpawns(b)) |> updateSpawns(a);
};

let consumeCapability = (capability, ambient) => {
  let (matches, rest) =
    List.partition(
      Capability.isEqual(capability),
      getCapabilities(ambient),
    );
  let firstRemoved = List.concat([List.tl(matches), rest]);
  switch (Capability.getNext(capability)) {
  | None => updateCapabilities(ambient, firstRemoved)
  | next => updateCapabilities(ambient, [next, ...firstRemoved])
  };
};

let consumeCapabilities = (capability1, capability2, a, b) => {
  (consumeCapability(capability1, a), consumeCapability(capability2, b));
};

let consumeSpawn = a => {
  Utils.mapWithDefault(List.tl, getSpawns(a), []) |> updateSpawns(a);
};

let create = (a, parent, capability, cb): ambient => {
  let c = findChild(getId(a), parent);
  let source = consumeCapability(capability, c);
  let target = getNextSpawn(source);
  cb({e: "create", t: getName(target)});
  let createInAmbient = (a, b) =>
    consumeSpawn(a)
    |> inheritChildren(b)
    |> inheritSpawns(b)
    |> inheritCapabilities(b);
  let createInRoot = (a, b) => consumeSpawn(a) |> addChild(b);
  switch (getName(target)) {
  | "" => createInAmbient(source, target)->updateChild(parent)
  | _ => createInRoot(source, target)->updateChild(parent)
  };
};

let enter = (a, b, parent, capability, cocapability, cb): ambient => {
  let c = findChild(getId(a), parent);
  let d = findChild(getId(b), parent);
  let (source, target) = consumeCapabilities(capability, cocapability, c, d);
  /* Add the entering ambient to the target ambient */
  cb({e: "in", t: getName(target)});
  let updated = addChild(source, target);
  /* Remove the entering ambient from its parent */
  parent |> removeChild(source) |> updateChild(updated);
};

let exit = (a, b, parent, capability, cocapability, cb): ambient => {
  let c = findChild(getId(a), parent);
  let d = findChild(getId(b), c);
  let (target, source) = consumeCapabilities(capability, cocapability, c, d);
  cb({e: "out", t: getName(target)});
  /* Add the exiting ambient to its target ambient */
  let updated = target |> removeChild(source);
  /* Remove the entering ambient from its parent */
  parent |> addChild(source) |> updateChild(updated);
};

let open_ = (a, b, parent, capability, cocapability, cb): ambient => {
  /* Find the child from the parent,
     if we can't find it assume 'a' is the root ambient */
  let c = Utils.mapWithDefault(findChild(getId(a)), parent, parent);
  let d = findChild(getId(b), c);
  let (source, target) = consumeCapabilities(capability, cocapability, c, d);
  let updateInParent = (parent, ambient) =>
    isEqual(ambient, parent) ? ambient : parent |> updateChild(ambient);

  cb({e: "open_", t: getName(target)});
  /* Remove the opening ambient from its parent (source),
     inherit the children and capabilities of the opened ambient and
     update the parent of the ambient that opened the target ambient */
  let updated =
    source
    |> removeChild(target)
    |> inheritChildren(target)
    |> inheritCapabilities(target)
    |> inheritSpawns(target)
    |> updateInParent(parent);

  /* "Create" is a special case in that it should be applied as soon
     as the previous capability has been consumed. TODO: is this wanted? */
  switch (Capability.getNext(capability)) {
  | Create => create(a, updated, Capability.Create, cb)
  | _ => updated
  };
};

let applyTransition = (parent, transition: transition(ambient), cb) => {
  let {Transition.source, target, capability, cocapability} = transition;
  switch (capability) {
  | Create => create(source, parent, capability, cb)
  | In(_) => enter(source, target, parent, capability, cocapability, cb)
  | Out_(_) => exit(source, target, parent, capability, cocapability, cb)
  | Open(_) => open_(source, target, parent, capability, cocapability, cb)
  | _ => parent
  };
};

let rec applyTransitionsRecursive = (ambient, cb: 'a => unit): ambient => {
  let reducer = (res, child: ambient) =>
    updateChild(applyTransitionsRecursive(child, cb), res);
  let updated1 = List.fold_left(reducer, ambient, getChildren(ambient));
  let applyWithCb = (res, child) => applyTransition(res, child, cb);
  let updated2 =
    List.fold_left(applyWithCb, updated1, getTransitions(updated1));
  updateTransitions(updated2, []);
};

let rec canReduce = ambient => {
  let hasTransitions = a => List.length(getTransitions(a)) > 0;
  let recursiveReducer = (res, acc) =>
    res || hasTransitions(acc) || canReduce(acc);
  hasTransitions(ambient)
  || List.fold_left(recursiveReducer, false, getChildren(ambient));
};

/* Summary of the reduction logic:
   - Traverse the ambient structure (tree) and gather all possible reductions (transitions)
   - Check if there are any reductions that can be made, if not, the ambient is fully reduced
   - If there are reductions to make, recursively traverse the ambient structure (tree) and apply the reductions (transitions)
   */
let rec reduceFully = ambient => {
  let transitionTree = AmbientTransitionTree.createRecursive(ambient);
  canReduce(transitionTree)
    ? applyTransitionsRecursive(transitionTree, Js.log) |> reduceFully
    : ambient;
};

let rec reduceFullyDebug = (index, cb: 'a => unit, ambient) => {
  let transitionTree = AmbientTransitionTree.createRecursive(ambient);
  let canReduceFurther = canReduce(transitionTree);

  let prefix =
    canReduceFurther && index > 0
      ? "step " ++ string_of_int(index) ++ ":"
      : index == 0 ? "initial state:" : "final state:";
  print_string(prefix ++ "\n" ++ treeToString(transitionTree) ++ "\n");

  canReduceFurther
    ? applyTransitionsRecursive(transitionTree, cb)
      |> reduceFullyDebug(index + 1, cb)
    : ambient;
};
