#!/bin/bash

json_escape () {
  s=$(printf '%s' "$1" | python3 -c 'import json,sys; print(json.dumps(sys.stdin.read()))')
  sed 's/ /\\ /g' <<<"$s" # escape whitespaces
}

sam deploy --region=$AWS_REGION --no-confirm-changeset --no-fail-on-empty-changeset --template-file template.yml --parameter-overrides\
    ParameterKey=HTTPCLIENTLOGREQUEST,ParameterValue="$HTTPCLIENT_LOGREQUEST"\
    ParameterKey=HTTPCLIENTLOGRESPONSE,ParameterValue="$HTTPCLIENT_LOGRESPONSE"\
    ParameterKey=DBPATH,ParameterValue="$DB_PATH"\
    ParameterKey=FINANCECMBASEURI,ParameterValue="$FINANCE_CM_BASE_URI"\
    ParameterKey=FINANCECMAUTHENTICATIONPATH,ParameterValue="$FINANCE_CM_AUTHENTICATION_PATH"\
    ParameterKey=FINANCECMVALIDATIONPATH,ParameterValue="$FINANCE_CM_VALIDATION_PATH"\
    ParameterKey=FINANCECMHOMEPATH,ParameterValue="$FINANCE_CM_HOME_PATH"\
    ParameterKey=FINANCECMDOWNLOADPATH,ParameterValue="$FINANCE_CM_DOWNLOAD_PATH"\
    ParameterKey=FINANCECMTRANSACTIONPATH,ParameterValue="$FINANCE_CM_TRANSACTION_PATH"\
    ParameterKey=FINANCECMUSERNAME,ParameterValue="$FINANCE_CM_USERNAME"\
    ParameterKey=FINANCECMPASSWORD,ParameterValue="$FINANCE_CM_PASSWORD"\
    ParameterKey=FINANCECMOTPSESSION,ParameterValue="$FINANCE_CM_OTPSESSION"\
    ParameterKey=FINANCECMAPKID,ParameterValue="$FINANCE_CM_APKID"\
    ParameterKey=FINANCETRANSACTIONSDIR,ParameterValue="$FINANCE_TRANSACTIONS_DIR"\
    ParameterKey=FINANCECMACCOUNTS,ParameterValue="$(json_escape "$FINANCE_CM_ACCOUNTS")"\
    ParameterKey=FINANCES3TRANSACTIONSREGION,ParameterValue="$FINANCE_S3_TRANSACTIONS_REGION"\
    ParameterKey=FINANCES3TRANSACTIONSBUCKET,ParameterValue="$FINANCE_S3_TRANSACTIONS_BUCKET"\
    ParameterKey=FINANCES3TRANSACTIONSPUBLICKEY,ParameterValue="$FINANCE_S3_TRANSACTIONS_PUBLICKEY"\
    ParameterKey=FINANCES3TRANSACTIONSSECRETKEY,ParameterValue="$FINANCE_S3_TRANSACTIONS_SECRETKEY"\
    ParameterKey=FINANCES3TRANSACTIONSPREFIX,ParameterValue="$FINANCE_S3_TRANSACTIONS_PREFIX"\
    ParameterKey=FINANCESETUPVOLUMEMAXKEYS,ParameterValue=$FINANCE_SETUP_VOLUME_MAXKEYS\
    ParameterKey=FINANCEWAGESTATEMENTS,ParameterValue="$(json_escape "$FINANCE_WAGE_STATEMENTS")"\
    ParameterKey=HEATERSBASEURI,ParameterValue="$HEATERS_BASEURI"\
    ParameterKey=HEATERSUSERNAME,ParameterValue="$HEATERS_USERNAME"\
    ParameterKey=HEATERSPASSWORD,ParameterValue="$HEATERS_PASSWORD"\
    ParameterKey=DOMOTICZBASEURI,ParameterValue="$DOMOTICZ_BASEURI"\
    ParameterKey=DOMOTICZWSURI,ParameterValue="$DOMOTICZ_WSURI"\
    ParameterKey=DOMOTICZUSERNAME,ParameterValue="$DOMOTICZ_USERNAME"\
    ParameterKey=DOMOTICZPASSWORD,ParameterValue="$DOMOTICZ_PASSWORD"\
    ParameterKey=SHUTTERSCONFIG,ParameterValue="$(json_escape "$SHUTTERS_CONFIG")"\
    ParameterKey=BASICAUTHUSERNAME,ParameterValue="$BASIC_AUTH_USERNAME"\
    ParameterKey=BASICAUTHPASSWORD,ParameterValue="$BASIC_AUTH_PASSWORD"\
    --stack-name sreapi --s3-bucket sreapi-package --capabilities CAPABILITY_IAM
