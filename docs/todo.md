# Things to learn or investigate

| Subject               | Where  | Projects     | Phase | Why                                 |
| ----------------------| ------ |:-------------| ------| ----------------------------------- |
| monad transformers    | work   | all          | 1     | To save output of child process     |
| ps async handling     | work   | agent        | 1     | How to handle async programming     |
| ps FFI                | work   | agent        | 1     | To call node-dbus                   |
| micro architecture    | work   | all          | 1     | Design before coding                |
| node-dbus             | work   | agent        | 1     | To interact with dbus/subman        |
| reactive              | work   | agent, wf    | 1     | For asynchronous programming        |
| vertx core/web        | work   | wf, polarize | 1     | Controller back end                 |
| Websockets            | work   | agent, wf    | 1     | transport proto for services        |
| TLS node              | work   | agent        | 2     | secure messages for dbus agent      |
| TLS vertx             | free   | wf           | 2     | secure messages for microservices   |
| OrientDB              | free   | wf           | 1     | persistence for polarize, wf        |
| css/less              | free   | wf           | 1     | layout handling                     |
| DOM                   | free   | wf           | 1     | for understanding presentation      |
| ECMAScript 2016       | work   | agent, wf    | 1     | to write native methods             |
| docker                | work   | all          | 1     | containerize polarize and services  |
| WebGL                 | free   | wf           | 2     | Visualization                       |
| Machine Learning      | free   | work         | 2     | Logging analytics                   |
| cockpit               | work   | agent        | 1     | subman cockpit module testing       |

## Monad Transformers: RWS and continuations

Right now, I'm stuck on a problem which I (think) will require knowledge of monad transformers (or possibly lenses). The
problem is that the onData function in purescript has a type of:

```haskell
onData :: forall w eff
        . Readable w (err :: EXCEPTION | eff)             -- A Stream type which is readable like stdout
       -> (Buffer -> Eff (err :: EXCEPTION | eff) Unit)   -- a function which takes a Buffer and returns Eff of Unit
       -> Eff (err :: EXCEPTION | eff) Unit               -- returns an Eff of Unit
```

So the problem is that since onData takes a function that takes a Buffer but returns an Eff of Unit, how do I save the
data from the Buffer somewhere?  The handler function returns Unit.

I have explored the idea of passing in a closure.  The closed over variable could be something like an STRef or a Ref
(or perhaps a Writer monad value).  Each call of the handler would mutate the hidden closed over state.

```haskell
-- | Function to save data from a buffer into an Ref
onDataSave :: forall e
            . Ref String
           -> Buffer
           -> Eff ( ref :: REF, buffer :: BUFFER, console :: CONSOLE | e) Unit
onDataSave ref buff = do
  bdata <- toString UTF8 buff
  modified <- modifyRef ref \current -> current <> bdata
  --log $ "Data as of now:\n" <> modified
  pure unit
```

This seems to work, but how do I combine the Eff onData returns?  I think this is where transformers come into play.

## purescript async: purescript-aff, purescript-rx or continuation monads?

The problem in a nutshell is dealing with asynchronous programming.  Since javascript runs in a single thread (usually),
you don't want to write blocking code.  That's essentially what node is all about.  But suppose we do want or need to
have serialized functions.  In other words, one function is dependent on another function completing.  If we turned our
logic into a call graph, we would see a linear progression rather than functions running in parallel.  The normal way
to solve this is to have callbacks.  If I have two functions I want to run, foo and bar, but bar can only run once foo
has completed, there are two ways to do this:

```javascript
// synchronous
const times_two = (x) => x * 2;
const minus_four = (y) => y - 4;
let x = foo(10);
let y = bar(x);

// asynchronous
const times_two_async = (x, cb) => {
  let ans = x * 2;
  return cb(ans);
}
y = times_two_async(10, minus_four);
```

This asynchronous style is essentially a kind of Continuation Passing Style (or CPS).  When you only have 2 functions,
this is not too bad, but if you have several functions, or very complicated functions, this style of asynchronous
programming has its drawbacks.  

So, I need to investigate possible purescript solutions to this problem.  Purescript can do the CPS style of programming
just like any other.  From a very initial glance at the purescript book, it appears that their solution to callback hell
is to use the continuation monad.  The continuation monad in turn requires understanding monad transformers and how to
compose monads together.  I have finally read the chapter of this in purescript, and it's a bit hard to follow.  I think
the reactive style might be simpler, however, there may be situations that require transformers, so I probably need to
learn them anyway.

It also appears that the purescript-aff module has some promise-like ability, or the ability to defer the result of a
computation.  From my initial look at it, I can't get the types to line up properly though.  But, I think if I can get
the types to line up, then purescript-aff is the way to go.

Then there's also the possibility of using the reactive style with purescript-rx.  So I should investigate how to use
the purescript-rx.  This might be the best route if only because this style will be useable from Java as well.  If you
think about it, each function that has a dependency on another function to complete can be modeled by having the
dependent function watch for an onCompleted event on the dependancy function.  In effect, it would be like creating a
call graph explicitly, where the edge would be the receipt of an onCompleted event.  This means that each vertex (which
would be an Observable) needs to emit this onCompleted event.

Finally, just to make things more confusing, lenses appear to be another possible solution.  This is because from my
initial look at them, they are about more than getting/setting nested information from a data structure.  They also have
some of the usecases of transformers as well as folding/traversing.

### Real world problem: calling child processes

Currently, I'm facing a real world asynchronous problem and need to investigate how to best solve this problem.  The
problem is that I need to call multiple child programs (for example a git clone and a mkdir command).  The second
command has a dependency on the first finishing.  In other words, I need to serialize the 2 process one after the other.
But since this is running node, and node is asynchronous, the first command is run, and then the 2nd is run immediately
after without waiting for the first to finish.

So one way of solving this solving this problem is to have a callback.  If you need to serialize something like this
you create a function with a callback, and this is callback is called when the calling function is done:

```haskell
-- | Launch a child process along with a callback
launch :: forall a
        . Command
       -> (a -> CmdEff)
       -> a
       -> CmdEff
launch (Command {command, args, opts, process, output}) cb a = do
  cp <- spawn command args opts
  writeRef process (Just cp)
  -- The output is accumulated to cmd.output each time the data event is caught
  onData (stdout cp) (onDataSave output)
  -- On success, call
  onExit cp \exit -> case exit of
              Normally 0 -> cb a  -- On success, we call the callback (a -> CmdEff)
              Normally x -> log $ "command failed with ret code: " <> show x
              BySignal _ -> log $ "command failed due to signal: " <> (show exit)
```

This works and it works well if you only have 2 things to do.  But if you need to start chaining other functions with
each other, this gets ugly fast.

## purescript FFI

For the dbus agent, I will need to use a node dbus library.  This means I will probably need to write some native code
and then write glue code to call it.  The first step will be to write the wrapper code around node-dbus, so that I can
interface it with purescript.

There is a good article on [FFI with purescript][-ffi] and in the [purescript book on FFI][-ffi-book].

## microservice architecture

I can borrow a lot from pheidippides.  Some things will need to change, since pheidippides assumed a low-level network
interface, but we will use websockets for all our work here.  Ultimately, the transport mechanism should be abstracted
out so that messages are simply sent via an underlying transport, whether that transport is via network sockets, web
sockets, or even in-memory protocol (perhaps actors or CSP).

Instead of a client talking directly to a microservice, it needs to talk to a controller.  The controller will do the
work of figuring out who (ie which microservice) to delegate the actual work to.

## Learning Projects

### Subprocesses in purescript

One project is for work.  This will be a script that replaces our bash script that gets executed in a jenkins job.  I've
been working on this one for a little while already, and what's been stumping me is figuring out how to get the types to
line up properly.  This is mainly due to the asynchronous nature of node.  Check out the [emissary project][-emissary]
for what I have so far.

### FFI with dbus

After I've got that running, my next step will be playing around with dbus.  For this one, I will need to learn how to
do the FFI better.  I have a minimal understanding of FFI so far, and indeed, I a lot of my learning of purescript has
come from reading the source code of other modules.  

[-ffi]: http://www.purescript.org/learn/ffi/
[-ffi-book]: https://leanpub.com/purescript/read#leanpub-auto-the-foreign-function-interface
[-emissary]: https://github.com/rarebreed/emissary/tree/dev
