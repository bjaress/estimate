FROM fpco/stack-build-small:lts-16.26 as build

COPY package.yaml .
COPY stack.yaml .

# Grab dependencies into the Docker cache first
RUN stack build --dependencies-only
RUN stack test --dependencies-only

COPY app app/
COPY src src/
RUN stack --local-bin-path . build --copy-bins

COPY test test/
RUN stack test


FROM ubuntu:18.04

COPY --from=build /estimate .
ENTRYPOINT ["./estimate"]
USER nobody
