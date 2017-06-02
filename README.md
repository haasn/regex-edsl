# What the fuck?

I got frustrated with muennich/urxvt-perls' url-select's default URL matching
regex, but when I tried extending it I quickly realized regex is impossible to
maintain.

So instead, I wrote this EDSL to generate regex from a high-level description
in a readable programming language (Haskell). Observe the magic:

```
>>> url
\b(?:(?:https?|ftp|file|mailto|magnet):(?:[\w\-@;/?:&=%$+*~#\p{N}\p{S}]|\p{L}\p{M}*|[\w\-@;/?:&=%$+*~#.!',]|(?!\(\)\[])\p{P})*(?:\((?:[\w\-@;/?:&=%$+*~#\p{N}\p{S}]|\p{L}\p{M}*|[\w\-@;/?:&=%$+*~#.!',]|(?!\(\)\[])\p{P})*\)|\[(?:[\w\-@;/?:&=%$+*~#\p{N}\p{S}]|\p{L}\p{M}*|[\w\-@;/?:&=%$+*~#.!',]|(?!\(\)\[])\p{P})*]|[\w\-@;/?:&=%$+*~#\p{N}\p{S}]|\p{L}\p{M}*)+|www\.(?:[\w\-@;/?:&=%$+*~#\p{N}\p{S}]|\p{L}\p{M}*|[\w\-@;/?:&=%$+*~#.!',]|(?!\(\)\[])\p{P})*(?:\((?:[\w\-@;/?:&=%$+*~#\p{N}\p{S}]|\p{L}\p{M}*|[\w\-@;/?:&=%$+*~#.!',]|(?!\(\)\[])\p{P})*\)|\[(?:[\w\-@;/?:&=%$+*~#\p{N}\p{S}]|\p{L}\p{M}*|[\w\-@;/?:&=%$+*~#.!',]|(?!\(\)\[])\p{P})*]|[\w\-@;/?:&=%$+*~#\p{N}\p{S}]|\p{L}\p{M}*)+|(?:[\w\-@;/?:&=%$+*~#\p{N}\p{S}]|\p{L}\p{M}*|[\w\-@;/?:&=%$+*~#.!',]|(?!\(\)\[])\p{P})*(?:\((?:[\w\-@;/?:&=%$+*~#\p{N}\p{S}]|\p{L}\p{M}*|[\w\-@;/?:&=%$+*~#.!',]|(?!\(\)\[])\p{P})*\)|\[(?:[\w\-@;/?:&=%$+*~#\p{N}\p{S}]|\p{L}\p{M}*|[\w\-@;/?:&=%$+*~#.!',]|(?!\(\)\[])\p{P})*]|[\w\-@;/?:&=%$+*~#\p{N}\p{S}]|\p{L}\p{M}*)+(?:\.com|\.net|\.org|\.de|\.co\.uk))
```

Now isn't that much nicer than having to write all of that regex garbage by
hand?
