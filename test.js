const fs = require("fs")
const deserializer = require("./src/Deserializer.bs.js")
const vm = require("./src/AmbientReducer.bs.js")
const jsonFile = "./__tests__/fixtures/001.json"
const json = fs.readFileSync(jsonFile, "utf8")
const program = deserializer.fromJSON(json)

const IdentityProvider = require('orbit-db-identity-provider')
const IPFS = require('ipfs')
const Log = require('ipfs-log')
const ipfs = new IPFS({
  repo: './ipfs'
})

ipfs.on('ready', async () => {
  const identity = await IdentityProvider.createIdentity({ id: 'A'})
  const log = new Log(ipfs, identity)
  const appendEvent = async ([e, t]) => {
    await log.append({ event: e, target: t })
  }
  const result = vm.reduceFullyDebug(0, appendEvent.bind(this), program)

  setTimeout(() => Object.values(log.heads).map(e => console.log(e.hash)), 2000)
})
