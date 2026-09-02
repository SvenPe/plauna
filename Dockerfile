FROM node:22-bookworm-slim AS frontend
WORKDIR /usr/src/app
COPY package.json package-lock.json ./
RUN npm ci
COPY . ./
RUN npm run build

FROM clojure:temurin-25-tools-deps-bookworm-slim AS build
WORKDIR /usr/src/app
COPY . ./
COPY --from=frontend /usr/src/app/resources/public/css/tailwind.css resources/public/css/tailwind.css
COPY --from=frontend /usr/src/app/resources/public/js/vendor resources/public/js/vendor
# The test runner is a git dependency fetched from GitHub. With this image's git (2.39) GitHub
# answers the HTTP/2 upload-pack request with 401, which aborts the classpath build; HTTP/1.1
# works, so pin it for the build stage only.
RUN git config --global http.version HTTP/1.1
RUN clojure -M:test
RUN clojure -T:build uber

FROM eclipse-temurin:25-alpine
RUN addgroup -S plauna \
    && adduser -S -G plauna -h /app plauna \
    && mkdir -p /var/lib/plauna \
    && chown plauna:plauna /var/lib/plauna
COPY --from=build --chown=plauna:plauna /usr/src/app/target/plauna-standalone.jar /app/
EXPOSE 8080
WORKDIR /app
USER plauna
# 'exec' so the JVM replaces the shell and runs as PID 1: this gives it proper signal handling
# (clean shutdown, and SIGQUIT/kill -3 thread dumps go straight to the JVM).
CMD ["sh", "-c", "exec java -jar plauna-standalone.jar $PLAUNA_ARGS"]
