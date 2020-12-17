FROM alpine:3.12 as builder

RUN apk --no-cache add --repository http://dl-cdn.alpinelinux.org/alpine/edge/community \
        ca-certificates git ghc=8.8.4-r0 upx curl musl-dev gmp-dev zlib-dev pcre-dev libx11-dev libxcb-dev libxrandr-dev libx11-static libxcb-static libxscrnsaver-dev
RUN  curl -sSL https://github.com/commercialhaskell/stack/releases/download/v2.1.3/stack-2.1.3-linux-x86_64-static.tar.gz | tar xvz && \
    mv stack*/stack /usr/bin

COPY stack.yaml /mnt
COPY *.cabal /mnt
WORKDIR /mnt
RUN rm -rf ~/.stack &&  \
    stack config set system-ghc --global true && \
    stack setup && \
    stack install --split-objs --ghc-options="-fPIC -fllvm" --only-dependencies

COPY . /mnt

# Hack as no Xss static lib on alpine, we don't need it
RUN ar cru /usr/lib/libXss.a
RUN echo '  ld-options: -static -Wl,--unresolved-symbols=ignore-all' >> greenclip.cabal ; \
    stack install --split-objs --ghc-options="-fPIC -fllvm"
RUN upx --ultra-brute /root/.local/bin/greenclip



FROM alpine:3.12 as runner

WORKDIR /root
COPY --from=builder /root/.local/bin/greenclip .
RUN chmod +x ./greenclip

CMD ["./greenclip"]

