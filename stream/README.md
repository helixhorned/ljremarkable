Streaming the reMarkable 2 framebuffer
--------------------------------------

This directory contains a pair of scripts implementing periodic streaming of the reMarkable
2 framebuffer to the local host, which is then rendered.

- `rM-cat-fb.sh --max-fps {1-9}`
  + should be copied to the tablet into a directory of your choice, e.g. `/home/root/bin/`
  + requires no additional compiled software: expects only the Ash shell from BusyBox (uses
    features in Ash but not in POSIX) and utilities already present on the reMarkable
- `host-render-frames.sh`
  + just invokes `ffplay` of FFmpeg with appropriate arguments

Transport of the data can be provided by SSH. Thus, having copied the first script, a
command on the local host could be:

```
ssh root@remarkable bin/rM-cat-fb.sh --max-fps 3 | ./host-render-frames.sh
```

(Assuming `/etc/hosts` to contain an entry such as `10.11.99.1 remarkable`.)

Discussion
----------

### Problems due to use of debugging interfaces

In contrast to the first iteration of the tablet, the system configuration/software of the
reMarkable 2 does not provide a means to access the framebuffer using well-defined
interfaces. Typically, third-party software accesses a portion of memory of the running
`xochitl` process (the main, Qt-based application) using `/proc/<PID>/mem` (see `man 5
procfs`), having previously obtained an address at which the framebuffer is expected to
start, and assuming a certain pixel format. This has undesirable consequences:

- Updates of the system software require a review of whether the assumptions incorporated in
  the logic of obtaining the framebuffer are still adequate. We choose to make
  `rM-cat-fb.sh` proceed only after checking that the running rM system is of a version
  known to us.
- There is no immediately obvious way of presenting to a program running as *non*-root user
  a restricted view of the memory in question: just the region holding the framebuffer, and
  just for reading. Note that by running third-party software (such as this one) on the
  tablet as root, you are trusting it absolutely.

### Lack of knowledge of real framebuffer updates

We capture the full framebuffer once initially, and then after each expiry of the wait
period computed as the reciprocal of the `--max-fps` value. That we do not implement dynamic
or partial streaming is inefficient: typically, the tablet is used such that updates happen
either

- infrequently (e.g. changing a page in a document), or
- frequently, but on small regions (such as drawing with the pen).

Note that **some** framebuffer updates do not fall into the two categories above, being both
on a large (possibly whole) portion of the screen as well as relatively frequent. An example
is panning of a document with two fingers.

Acknowledgements
----------------

The [reStream](https://github.com/rien/reStream) project served as inspiration.

Disclaimer
----------

The *ljremarkable* project is not affiliated with, or endorsed by, reMarkable AS.
