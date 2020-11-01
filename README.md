IOT protocol library
=====

### Usage

#### Create message

##### Message Type: 1
```erlang
{ok, BinaryMsg} = iot_protocol:ensemble(iot_protocol_obj:new(1)).
```

##### Message Type: 1 with ProtocolVersion: 2 
```erlang
{ok, BinaryMsg} = iot_protocol:ensemble(iot_protocol_obj:new(2, 1)).
```

##### Message Type: 1 with Payload: <<"hello"">> 
```erlang
{ok, BinaryMsg} = iot_protocol:ensemble(iot_protocol_obj:new(1, <<"hello">>)).
```

##### Message Type: 1 with ProtocolVersion: 2 with Payload: <<"hello"">> 
```erlang
{ok, BinaryMsg} = iot_protocol:ensemble(iot_protocol_obj:new(2, 1, <<"hello">>)).
```

##### Set Audit Number
```erlang
NewObj = iot_protocol:set_audit_number(Obj, 2019090307501234).
```

##### Construct reply MessageType: 12 and Payload: <<"reply">>
```erlang
Reply = iot_protocol:reply(NewObj, 12, <<"reply">>).
```

#### Parse message

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

### Protocol description

* First 4 bytes is the total message length (4 byte length included)
* Fifth byte is the protocol version
* Sixth byte is the message type
* Seventh 5 bytes is the audit_number
* Payload


Build
-----

    $ rebar3 compile
    
Test 
----
    $ rebar3 test
