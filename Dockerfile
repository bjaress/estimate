FROM fpco/alpine-haskell-stack:8.10.4 as build

# project config (including ID of package index snapshot)
COPY stack.yaml .

# update package index
RUN stack update

# Dependency lists
COPY package.yaml .

# Placeholder
RUN echo 0.0.0 > version
# Fetch and compile dependencies
RUN stack build --system-ghc --flag estimate:static --dependencies-only
RUN stack test --system-ghc --dependencies-only

# Code
COPY src src/
COPY app app/
COPY version .

# Compile and save to /bin
RUN stack --local-bin-path bin build --copy-bins --system-ghc --flag estimate:static

COPY test test/
RUN stack --system-ghc test


FROM scratch

COPY --from=build /bin/estimate /

# Common user ID for "nobody"
# https://en.m.wikipedia.org/wiki/User_identifier#Special_values
USER 65534:65534

ENTRYPOINT ["/estimate"]
