contract Foo
{
    fallback() external payable
    {
        revert();
    }
}