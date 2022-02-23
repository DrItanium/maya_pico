Originally maya was a radical fork of clips but I realized that it was better
to keep the core of it the same and just work on creating a simple wrapper
around it instead. I got permission from work to open source the C++17 wrapper
library we use internally (LibElectron), this is different from the archived
LibElectron because this one is far more complete. To simplify the open
sourcing process, I have preserved the file structure we use internally for our
tools which use clips. We have designed the electron wrapper in such a way as
to make it more natural to interface with CLIPS from a C++17 context. 

We have also added functions and features from boost to CLIPS so that we have
more control over many extra features. We do throw exceptions but none of them
will cross the boundary between C and C++ unless it makes sense to do so (there
is a need to just terminate so throw the exception and hose the stack
for the C portion).
