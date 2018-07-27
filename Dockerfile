FROM alpine:3.8 as builder
MAINTAINER github@erebe.eu

RUN apk --no-cache add --repository http://dl-cdn.alpinelinux.org/alpine/edge/community \
        ca-certificates git ghc=8.4.3-r0 upx curl musl-dev gmp-dev zlib-dev pcre-dev libx11-dev libxrandr-dev
RUN curl -L https://github.com/commercialhaskell/stack/releases/download/v1.6.5/stack-1.6.5-linux-x86_64-static.tar.gz | tar -xz ; \
    cp stack-1.6.5-linux-x86_64-static/stack /usr/bin/

COPY stack.yaml /mnt
COPY *.cabal /mnt
WORKDIR /mnt
RUN rm -rf ~/.stack &&  \
    stack config set system-ghc --global true && \
    stack setup && \
    stack install --split-objs --ghc-options="-fPIC -fllvm" --only-dependencies

COPY . /mnt

RUN echo '  ld-options: -static -Wl,--unresolved-symbols=ignore-all' >> greenclip.cabal ; \
    stack install --split-objs --ghc-options="-fPIC -fllvm"
RUN upx --ultra-brute /root/.local/bin/greenclip



FROM alpine:3.8 as runner
MAINTAINER github@erebe.eu

WORKDIR /root
COPY --from=builder /root/.local/bin/greenclip .
RUN chmod +x ./greenclip

CMD ["./greenclip"]

