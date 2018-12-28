const { createService, getService, deleteService } = require('./service');
const { createLocalDb } = require('./createLocalDb');
const { getAppVersion } = require('./getAppVersion');

module.exports = {
    createService,
    getService,
    deleteService,
    createLocalDb,
    getAppVersion,
};
