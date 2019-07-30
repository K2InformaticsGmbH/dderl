FROM alpine:3.9

ENV OTP_VERSION="20.2"

RUN set -xe \
	&& apk add --no-cache --virtual .fetch-deps \
		curl \
		ca-certificates \
	&& curl -fSL -o otp-src.tar.gz "http://www.erlang.org/download/otp_src_20.2.tar.gz" \
	&& apk add --no-cache --virtual .build-deps \
		dpkg-dev dpkg \
		gcc \
		g++ \
		libc-dev \
		linux-headers \
		make \
		autoconf \
		ncurses-dev \
		openssl-dev \
		unixodbc-dev \
		lksctp-tools-dev \
		tar \
    nodejs \
    git \
    yarn \
	&& export ERL_TOP="/usr/src/otp_src_${OTP_VERSION%%@*}" \
	&& mkdir -vp $ERL_TOP \
	&& tar -xzf otp-src.tar.gz -C $ERL_TOP --strip-components=1 \
	&& rm otp-src.tar.gz \
	&& ( cd $ERL_TOP \
	  && ./otp_build autoconf \
	  && gnuArch="$(dpkg-architecture --query DEB_BUILD_GNU_TYPE)" \
	  && ./configure --build="$gnuArch" \
	  && make -j$(getconf _NPROCESSORS_ONLN) \
	  && make install ) \
	&& rm -rf $ERL_TOP \
	&& find /usr/local -regex '/usr/local/lib/erlang/\(lib/\|erts-\).*/\(man\|doc\|obj\|c_src\|emacs\|info\|examples\)' | xargs rm -rf \
	&& find /usr/local -name src | xargs -r find | grep -v '\.hrl$' | xargs rm -v || true \
	&& find /usr/local -name src | xargs -r find | xargs rmdir -vp || true \
	&& scanelf --nobanner -E ET_EXEC -BF '%F' --recursive /usr/local | xargs -r strip --strip-all \
	&& scanelf --nobanner -E ET_DYN -BF '%F' --recursive /usr/local | xargs -r strip --strip-unneeded \
	&& runDeps="$( \
		scanelf --needed --nobanner --format '%n#p' --recursive /usr/local \
			| tr ',' '\n' \
			| sort -u \
			| awk 'system("[ -e /usr/local/lib/" $1 " ]") == 0 { next } { print "so:" $1 }' \
	)" \
	&& REBAR3_DOWNLOAD_URL="https://s3.amazonaws.com/rebar3/rebar3" \
	&& curl -fSL -o rebar3 "$REBAR3_DOWNLOAD_URL" \
	&& mkdir -p /usr/src/rebar3-src \
	&& mv rebar3 /usr/src/rebar3-src \
	&& cd /usr/src/rebar3-src \
	&& install -v ./rebar3 /usr/local/bin/ \
	&& rm -rf /usr/src/rebar3-src \
	&& apk add --virtual .erlang-rundeps \
		$runDeps \
		lksctp-tools \
		ca-certificates

CMD ["erl"]
