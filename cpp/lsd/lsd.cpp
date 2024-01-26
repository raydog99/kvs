#include "server.h"
#include <capnp/serialize-packed.h>
#include <capnp/rpc-twoparty.h>
#include <kj/debug.h>
#include <kj/compat/http.h>
#include <kj/async.h>
#include <vector>
#include <thread>

class LocationServiceDaemon {
public:
    LocationServiceDaemon() : server(rpcSystem), bootstrap(rpcSystem, server) {}

    void start() {
        std::cout << "Chord Location Service Daemon started." << std::endl;
        kj::NEVER_DONE.wait(rpcSystem.getWaitScope());
    }
};

int main() {
    LocationServiceDaemon locationService;
    locationService.start();
    return 0;
}
