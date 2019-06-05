FROM openjdk:8-jdk-alpine

ARG JAR_FILE

ADD ${JAR_FILE} sre-api.jar

ENTRYPOINT java $JAVA_OPTIONS $APP_OPTIONS -Djava.security.egd=file:/dev/./urandom -jar sre-api.jar

EXPOSE 8080
