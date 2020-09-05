IOT protocol library
=====

### Usage

#### Create message

##### Message Type: 1
```erlang
BinaryMsg = iot_protocol:ensemble(iot_protocol_obj:new(1)).
```

##### Message Type: 1 with ProtocolVersion: 2 
```erlang
BinaryMsg = iot_protocol:ensemble(iot_protocol_obj:new(2, 1)).
```

##### Message Type: 1 with Payload: <<"hello"">> 
```erlang
BinaryMsg = iot_protocol:ensemble(iot_protocol_obj:new(1, <<"hello">>)).
```

##### Message Type: 1 with ProtocolVersion: 2 with Payload: <<"hello"">> 
```erlang
BinaryMsg = iot_protocol:ensemble(iot_protocol_obj:new(2, 1, <<"hello">>)).
```

##### Set Audit Number
```erlang
NewObj = iot_protocol:set_audit_number(Obj, 2019090307501234).
```

##### Construct reply MessageType: 12 and Payload: <<"reply">>
```erlang
Reply = iot_protocol:reply(NewObj, 12, <<"reply">>).
```

#### parse message

```erlang
Obj = io_protocol:parse(<<0, 0, 0, 0, 1, 5, 0, 0, 0, 0, 0>>).
```

##### Get message type
```erlang
iot_protocol_obj:get_message_type(Obj)
```

##### Get protocol version
```erlang
iot_protocol_obj:get_protocol_version(Obj)
```

##### Get payload
```erlang
iot_protocol_obj:get_payload(Obj)
```

##### Get audit number
```erlang
iot_protocol_obj:get_audit_number(Obj)
```


Build
-----

    $ rebar3 compile
    
Test 
----
    $ rebar3 test
