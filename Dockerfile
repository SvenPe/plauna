FROM clojure:temurin-25-tools-deps-bookworm-slim as build
RUN apt update && apt install -y nodejs npm
COPY . /usr/src/app/
WORKDIR /usr/src/app
RUN npm install
RUN npm run build
# FIXME Tests are currently disabled because some of them fail at random due to concurrency
# RUN clojure -M:test
RUN clojure -T:build uber

FROM eclipse-temurin:25-alpine
COPY --from=build /usr/src/app/target/plauna-standalone.jar /app/
EXPOSE 8080
WORKDIR /app
RUN mkdir /var/lib/plauna # Default location for data files
# 'exec' so the JVM replaces the shell and runs as PID 1: this gives it proper signal handling
# (clean shutdown, and SIGQUIT/kill -3 thread dumps go straight to the JVM).
CMD ["sh", "-c", "exec java -jar plauna-standalone.jar $PLAUNA_ARGS"]
