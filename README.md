# Test.Spec.Halogen

A Purescript module that adds utilities for automated Halogen component testing.

```haskell
spec :: Spec Unit
spec = withComponent componentSpec input do
  describe "My Component" do
    it "should trigger internal actions" $ ReaderT do
      trigger (Action.AddUser user)
    it "should detect outputs" $ ReaderT do
      trigger (Action.AddUser user) `shouldRaise` Output.AddedUserSuccessfully
    it "can provide access to state" $ ReaderT do
      trigger (Action.AddUser user)
      stateShouldSatisfy \s -> s.numUsers == 1
```

Components are created and destroyed during every test

## Linguistic choices

A Halogen component has a large number of type variables that constrain operations on the component.

- `state` is the component's state
- `query` is the query algebra; the requests that can be made of the component
- `action` is the type of actions; messages internal to the component that can be evaluated
- `slots` is the set of slots for addressing child components
- `input` is the input value that will be received when the parent of this component renders
- `output` is the type of messages the component can raise

When a `ComponentSpec` is reified into a `Component`, most of these type variables become hidden:

`mkComponent :: ComponentSpec state query action slots input output m -> Component query input output m`

But during testing, we want to be able to trigger actions from outside

| Halogen | Spec-Halogen |
| -- | -- |
| `query` | `AugmentedQuery state query action slots input output` |
| `output` | `AugmentedOutput state query action slots input output` |
| `Component query input output` | `TestComponent state query action slots input output` |
| `HalogenIO query output m` | `TestHalogenIO state query action slots input output m` |

The Spec-Halogen equivalents expose the

## Usage

Every operation performed on a TestComponent has a [collocated verb](https://ellii.com/blog/verb-collocations) with a past and present tense.

The **present tense** is used when performing an operation.  The **past tense** creates predicates used to test that an operation was induced.

| Operation type | Present tense | Past tense |
| -- | -- | -- |
| State | `modify state` |  `modified state` |
| Query | `request query` |  `requested query` |
| Action | `trigger action` | `triggered action` |
| Input | `recieve input` |  N/A |
| Ouput | N/A | `raised output` |




# Prepping Component for testing

## Expose `ComponentSpec` in module

When testing, we use `mkTestComponent` to create a thin wrapper over the original component that provides access to internal `Action` and `State`. The spec used to create the component should also be exported from the module.

```haskell
module MyComponent (component, componentSpec)

component :: Component q i o m
component = mkComponent myComponentSpec

componentSpec :: ComponentSpec 
ComponentSpec s q a sl i o m = ...
```


## Create instances for `Action` and `Output` and ???
To use the assertions defined in `Test.Spec.Halogen.Assertions`, we need to be able to test `Action` and `Output` for equality. These instances can be created automatically by using `derive instance Eq _`.

```haskell
data Action = Action1 | Action2
derive instance Eq Action

data Output = Finished | Flailing
derive instance Eq Output
```

A `Show` instance for `Action` and `Output` may also be required. Write your tests and define `Show` instances when the compiler complains. `Show` instances can also be derived via `Generic`.

```haskell
derive instance Generic Action _
instance Show Action where
  show = genericShow
```

# Writing tests

```haskell
withComponent :: ...
  TestComponent ...
  -> input
  -> SpecT Aff TestHalogenConfig m a
  -> SpecT Aff Unit m a
```


```haskell
withComponent (mkTestComponent componentSpec) input do
  describe "My Component" do
    it "test1" \testConfig ->
      


```
## I want to test for more complex patterns of behavior!

Because `IncrementalPredicate` has a `HeytingAlgebra` instance, they can be used with standard boolean algebra functions:

```haskell
do
  -- passes when either a is 
  action1 *> shouldTrigger (a || b)
  action2 *> shouldTrigger (raised Failing && raised Flailing)
```


## I want to preserve component state between tests!

## I want to define properties that

## My component needs more time to process input!