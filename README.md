gore-and-ash-logging
====================

The module provides facilities for console logging for [Gore&Ash](https://github.com/Teaspot-Studio/gore-and-ash) engine.

Installing
==========

Add following to your `stack.yml` to `packages` section:
```yaml
- location:
    git: https://github.com/Teaspot-Studio/gore-and-ash-logging.git
    commit: <PLACE HERE FULL HASH OF LAST COMMIT> 
```

When defining you application stack, add `LoggingT`:
``` haskell
type AppStack = ModuleStack [LoggingT, ... other modules ... ] IO
```

And derive `LoggingMonad` for your resulting `AppMonad`:
``` haskell
newtype AppMonad a = AppMonad (AppStack a)
  deriving (Functor, Applicative, Monad, MonadFix, MonadIO, LoggingMonad)
```