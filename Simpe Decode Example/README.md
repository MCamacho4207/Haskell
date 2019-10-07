This is a small function which looks for specific characters in a string and decode a message with a simple rule. 

For example, "H0W 1S 0UR F0UND1NG J1M" translates to "bad" using the rule that if you take the 0's and 1's and put them together:

"010011"

And then use the code:

"00" = 'a'

"01" = 'b'

"10" = 'c'

"11" = 'd'

To reveal the message:

'b' 'a' 'd' = "bad"

Function is called with "extractMessage s" where s is the string to be decoded.
