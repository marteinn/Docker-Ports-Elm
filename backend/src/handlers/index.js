const { createService, getService, deleteService, updateService } = require('./service');
const { createLocalDb } = require('./createLocalDb');
const { getAppVersion } = require('./getAppVersion');

module.exports = {
    createService,
    getService,
    deleteService,
    updateService,
    createLocalDb,
    getAppVersion,
};
