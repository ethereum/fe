contract SimpleContract:
    pub def simple_function(x: u256) -> u256:
        return x
---
{
    object "SimpleContract" {
        function simple_function(x:u256) -> return_val {
            return_val := x
        }
    }
}
