FROM openjdk:8-jdk-alpine

ARG JAR_FILE

ADD ${JAR_FILE} dashboard-server.jar

ENTRYPOINT java $JAVA_OPTIONS -Djava.security.egd=file:/dev/./urandom -jar dashboard-server.jar

EXPOSE 8080
