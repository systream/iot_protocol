IOT protocol library
=====

### Usage

#### create message

```erlang
io_protocol:ensemble(iot_protocol_obj:new(1))
```

#### parse message

```erlang
Obj = io_protocol:parse(<<0,0,0,0,1,5>>),
iot_protocol_obj:get_message_type(Obj)

```

Build
-----

    $ rebar3 compile
