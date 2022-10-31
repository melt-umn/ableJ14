
This is legacy code for an extensible version of Java 1.4.
It builds using [Silver 0.4.5](https://github.com/melt-umn/silver/releases/tag/v0.4.5).

This was written before the [modular well-definedness analysis (MWDA)](https://www-users.cse.umn.edu/~evw/pubs/kaminski12sle/kaminski12sle.pdf) was developed, which is used to ensure grammars will compose without needing user intervention.
Because this predates MWDA, it does not follow these ideas and does not pass Silver's flow analysis that checks for adherence to MWDA.
For better examples of large extensible languages written in Silver, see [AbleC](https://github.com/melt-umn/ableC).

## Notes

This repo has been mildly modernized in its file organization, but mostly left as it was. There are some scattered non-Silver files under `edu...` though:

```
find edu.umn.cs.melt.ableJ14 -type f | grep -v '.sv$'
```

