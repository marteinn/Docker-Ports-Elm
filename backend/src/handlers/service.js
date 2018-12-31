"use strict";

const AWS = require("aws-sdk");
const R = require("ramda");
const uuidv4 = require("uuid/v4");
const { getSettings } = require("../settings.js");
const { withOfflineSupport } = require("../decorators.js");

const CORS_HEADERS = {
    "Access-Control-Allow-Origin": "*",
    "Access-Control-Allow-Credentials": true
};


const createService = async (event, context) => {
    const {
        dockerPort, project, name, ...extraValues
    } = JSON.parse(event.body);

    if (!dockerPort || !project) {
        return buildErrorResponse({
            message: "Missing either dockerPort or project",
            code: "MissingArguments"
        });
    }

    const services = await queryDoc({ TableName: getSettings().TABLE_NAME }, {
        KeyConditionExpression: 'dockerPort = :dockerPort',
        ExpressionAttributeValues: {
            ':dockerPort': dockerPort,
        }
    })

    if (services.length) {
        return buildErrorResponse({
            message: "Port already assigned",
            code: "PORT_ALREADY_ASSIGNED"
        });
    }

    const cleanedExtraValues = R.filter(R.pipe(R.isEmpty, R.not), extraValues)
    const model = {
        ...cleanedExtraValues,
        dockerPort,
        project,
        name,
        created: new Date().getTime()
    };
    try {
        await putDoc({ TableName: getSettings().TABLE_NAME }, model)
        return {
            statusCode: 201,
            headers: CORS_HEADERS,
            body: JSON.stringify({
                ...model,
                comment: R.propOr('', 'comment', model),
                project: R.propOr('', 'project', model)
            })
        };
    } catch (err) {
        return buildErrorResponse(err);
    }
};

const buildErrorResponse = err => {
    return {
        statusCode: err.statusCode,
        headers: CORS_HEADERS,
        body: JSON.stringify({
            message: err.message,
            code: err.code
        })
    };
};

const putDoc = async (config, item) => {
    let docClient = new AWS.DynamoDB.DocumentClient();
    const model = { ...config, Item: item };
    return docClient.put(model).promise();
};

const getDoc = async (config, params) => {
    const docClient = new AWS.DynamoDB.DocumentClient();
    const options = { ...config, Key: params }
    return docClient.get(options).promise();
}

const queryDoc = async (config, params) => {
    const docClient = new AWS.DynamoDB.DocumentClient();
    const options = { ...config, ...params };
    const resp = await docClient.query(options).promise();
    return R.prop('Items', resp)
}

const deleteDoc = async (config, params) => {
    const docClient = new AWS.DynamoDB.DocumentClient();
    const options = { ...config, Key: params }
    return docClient.delete(options).promise();
}

const scanDoc = async (config, params = {}) => {
    const docClient = new AWS.DynamoDB.DocumentClient();
    const options = { ...config, ...params };
    const resp = await docClient.scan(options).promise()
    return R.prop('Items', resp)
}

const getService = async (event, context) => {
    try {
        let items = await scanDoc({
            TableName: getSettings().TABLE_NAME
        });
        items = items.map(x => ({
            ...x,
            comment: R.propOr('', 'comment', x),
            project: R.propOr('', 'project', x)
        }), items)

        return {
            statusCode: 200,
            body: JSON.stringify(items)
        };
    } catch (err) {
        return buildErrorResponse(err);
    }
};

const updateService = async (event, context) => {
    let { dockerPort } = event.pathParameters
    const {
        project, name, ...extraValues
    } = JSON.parse(event.body);

    dockerPort = Number(dockerPort)

    const services = await queryDoc({ TableName: getSettings().TABLE_NAME }, {
        KeyConditionExpression: 'dockerPort = :dockerPort',
        ExpressionAttributeValues: {
            ':dockerPort': dockerPort,
        }
    })

    if (!services.length) {
        return buildErrorResponse({
            message: "Service was not found",
            code: "PORT_NOT_FOUND"
        });
    }

    try {
        const service = R.head(services)
        await deleteDoc({ TableName: getSettings().TABLE_NAME }, {
            dockerPort: service.dockerPort,
            project: service.project,
        })

        const cleanedExtraValues = R.filter(R.pipe(R.isEmpty, R.not), extraValues)
        const model = {
            ...cleanedExtraValues,
            dockerPort,
            project,
            name,
            created: new Date().getTime()
        };
        await putDoc({ TableName: getSettings().TABLE_NAME }, model)

        return {
            statusCode: 201,
            body: JSON.stringify({
                ...model,
                comment: R.propOr('', 'comment', model),
                project: R.propOr('', 'project', model)
            })
        }
    } catch (err) {
        return buildErrorResponse(err);
    }
}

const deleteService = async (event, context) => {
    let { dockerPort } = event.pathParameters
    dockerPort = Number(dockerPort)

    const services = await queryDoc({ TableName: getSettings().TABLE_NAME }, {
        KeyConditionExpression: 'dockerPort = :dockerPort',
        ExpressionAttributeValues: {
            ':dockerPort': dockerPort,
        }
    })

    if (!services.length) {
        return buildErrorResponse({
            message: "Service was not found",
            code: "PORT_NOT_FOUND"
        });
    }
    const service = R.head(services)

    await deleteDoc({ TableName: getSettings().TABLE_NAME }, {
        dockerPort: service.dockerPort,
        project: service.project,
    })

    return {
        statusCode: 200,
    };
}

module.exports = {
    createService: withOfflineSupport(createService),
    getService: withOfflineSupport(getService),
    deleteService: withOfflineSupport(deleteService),
    updateService: withOfflineSupport(updateService),
};
