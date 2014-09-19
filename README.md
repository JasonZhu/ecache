Erlang In-Memory Cache
===========================

**ecache** is a erlang in-memory TTL cache base on ETS.

## Usage

Just add `ecache` dep to you `rebar.config`:

```erlang
{deps, [
    {ecache, ".*", 
      {git, "https://github.com/JasonZhu/ecache.git", {branch, "master"}}}
  ]}.
```

And `ecache` settings in your app.config file:

```erlang
{ecache,[
    {ets_threshold, 0.85}, 
    {ets_maxsize, 209715200} %% 200*1024*1024 = 200M
  ]}
```
## API
```erlang

ecache:set(Key, Val, TTL) -> ok.

ecache:get(Key) -> {ok, Val} | {error, Reason}.

ecache:delete(Key) -> ok.
```

## Features

## RoadMap
