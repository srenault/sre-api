import { APIGatewayRequestAuthorizerEvent } from "aws-lambda";

declare module AWS {
  const policyVersion: string;
}

const USERNAME: string | undefined = process.env["BASIC_AUTH_USERNAME"];
const PASSWORD: string | undefined = process.env["BASIC_AUTH_PASSWORD"];

exports.handler = async (event: APIGatewayRequestAuthorizerEvent) => {
  const authorizationHeader = event.headers?.authorization;

  if (!USERNAME) {
    throw new Error('Missing "username" environment variable');
  }

  if (!PASSWORD) {
    throw new Error('Missing "password" environment variable');
  }

  if (!authorizationHeader) {
    return {
      isAuthorized: false,
      context: {},
    };
  }

  const [, encodedCreds] = authorizationHeader.split(" ");

  const [username, password] = decodeBase64(encodedCreds).split(":");

  const isAuthorized = username === USERNAME && password === PASSWORD;

  return {
    isAuthorized,
    context: {},
  };
};

function decodeBase64(s: string) {
  return Buffer.from(s, "base64").toString("ascii");
}
