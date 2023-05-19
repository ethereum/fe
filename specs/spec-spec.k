module SPEC-SPEC 
    imports VERIFICATION

    claim

    <kevm>
        <k> #execute => #halt </k>
        <exit-code> 1 </exit-code>
        <mode> NORMAL </mode>
        <schedule> ISTANBUL </schedule>

        <ethereum>
            <evm>
                <output> _ => #buf(0, 0) </output>
                <statusCode> _ => EVMC_SUCCESS </statusCode>
                <callStack> _ </callStack>
                <interimStates> _ </interimStates>
                <touchedAccounts> _ => ?_ </touchedAccounts>

                <callState>
                    // todo: gen this part
                    <program> #parseByteStack("0x6001601a43111460011460205760016019431414600114601b57005b600080fd5b00") </program>
                    <jumpDests> #computeValidJumpDests(#parseByteStack("0x6001601a43111460011460205760016019431414600114601b57005b600080fd5b00")) </jumpDests>

                    <id> ACCT_ID </id> // contract owner
                    <caller> CALLER_ID </caller> // who called this contract; in the beginning, origin // msg.sender

                    // todo: gen this part
                    <callData> #buf(32, ARG_0) </callData>

                    <callValue> 0 </callValue>
                    <wordStack> .WordStack => ?_ </wordStack>
                    <localMem> .Bytes => ?_ </localMem>
                    <pc> 0 => ?_ </pc>
                    <gas> #gas(_VGAS) => ?_ </gas>
                    <memoryUsed> 0 => ?_ </memoryUsed>
                    <callGas> _ => ?_ </callGas>

                    <static> false </static> // NOTE: non-static call
                    <callDepth> CALL_DEPTH </callDepth>
                </callState>

                <substate>
                    <selfDestruct> _ </selfDestruct>
                    <log> _ </log>
                    <refund> _ </refund>
                    <accessedAccounts> _ => ?_ </accessedAccounts>
                    <accessedStorage> _ => ?_ </accessedStorage>
                </substate>

                <gasPrice> _ </gasPrice>
                <origin> ORIGIN_ID </origin>

                <blockhashes> _ </blockhashes>
                <block>
                    <previousHash> _ </previousHash>
                    <ommersHash> _ </ommersHash>
                    <coinbase> _ </coinbase>
                    <stateRoot> _ </stateRoot>
                    <transactionsRoot> _ </transactionsRoot>
                    <receiptsRoot> _ </receiptsRoot>
                    <logsBloom> _ </logsBloom>
                    <difficulty> _ </difficulty>
                    <number> _ </number>
                    <gasLimit> _ </gasLimit>
                    <gasUsed> _ </gasUsed>
                    <timestamp> _ </timestamp>
                    <extraData> _ </extraData>
                    <mixHash> _ </mixHash>
                    <blockNonce> _ </blockNonce>
                    <ommerBlockHeaders> _ </ommerBlockHeaders>
                    <baseFee> _ </baseFee>
                </block>
            </evm>

            <network>
                <chainID> _ </chainID>

                <activeAccounts> SetItem(ACCT_ID) _:Set </activeAccounts>

                <accounts>
                    <account>
                        <acctID> ACCT_ID </acctID>
                        <balance> _ </balance>
                        <code> #parseByteStack("0x6001601a43111460011460205760016019431414600114601b57005b600080fd5b00") </code>
                        <storage> _ </storage>
                        <origStorage> _ </origStorage>
                        <nonce> _ </nonce>
                    </account>
                </accounts>

                <txOrder> _ </txOrder>
                <txPending> _ </txPending>
                <messages> _ </messages>
            </network>
        </ethereum>
    </kevm>

    requires 0 <=Int ACCT_ID    andBool ACCT_ID    <Int (2 ^Int 160)
        andBool 0  <=Int CALLER_ID  andBool CALLER_ID  <Int (2 ^Int 160)
        andBool 0  <=Int ORIGIN_ID  andBool ORIGIN_ID  <Int (2 ^Int 160)
        andBool 0  <=Int CALL_DEPTH andBool CALL_DEPTH <Int 1024
        andBool 0 ==Int ARG_0

endmodule

