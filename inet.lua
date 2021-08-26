-- SPDX-License-Identifier: MIT

local ffi = require("ffi")
local C = ffi.C

local bit = require("bit")

local class = require("class").class
local error_util = require("error_util")
local posix = require("posix")
local posix_decls = require("posix_decls")

local check = error_util.check
local checktype = error_util.checktype

local assert = assert
local error = error
local unpack = unpack

----------

local api = {}

-- Notes:
--  * In the real 'struct sockaddr_in', <port> and <address> are integers that are expected
--    in network byte order.
--  * The value '14' is from 'man 2 bind'.
local SockAddrInet = ffi.typeof[[
struct {
    sa_family_t family;
    uint8_t port[2];
    uint8_t address[4];

    uint8_t padding_[14 - 6];
} __attribute__((aligned(4)))
]]

local const_sockaddr_ptr_t = ffi.typeof("const struct sockaddr *")

local function cast(sockAddr)
    assert(ffi.istype(SockAddrInet, sockAddr))
    return ffi.cast(const_sockaddr_ptr_t, sockAddr)
end

local function SocketAddress(addrAndPort)
    checktype(addrAndPort, 1, "table", 2)
    check(#addrAndPort == 5, "argument #1 must have length 5", 2)

    local port = addrAndPort[5]
    local quad = { unpack(addrAndPort, 1, 4) }

    local portBytes = {
        bit.rshift(bit.band(port, 0xff00), 8),
        bit.band(port, 0xff)
    }

    return SockAddrInet(posix.AF.INET, portBytes, quad)
end

local AF = posix.AF
local SOCK = posix.SOCK

api.Socket = class
{
    function()
        local fd = C.socket(AF.INET, SOCK.STREAM, 0)

        if (fd < 0) then
            error("Failed creating socket: "..posix.getErrnoString())
        end

        return {
            -- NOTE: closed only from posix.Fd via one of the two connection paths.
            fd = fd
        }
    end,

    initiateConnection = function(self, addrAndPort)
        local sockAddr = SocketAddress(addrAndPort)
        local ret = C.connect(self.fd, cast(sockAddr), ffi.sizeof(sockAddr))

        if (ret < 0) then
            return nil, posix.getErrnoString()
        end

        return posix.Fd(self.fd)
    end,

    expectConnection = function(self, port, timeoutMs, beforeFirstAccept)
        local E, F, POLL, O = posix_decls.E, posix_decls.F, posix.POLL, posix.O

        -- From /usr/include/netinet/in.h:
        -- /* Address to accept any incoming messages.  */
        -- #define INADDR_ANY ((in_addr_t) 0x00000000)
        local bindAddr = SocketAddress{0,0,0,0; port}

        if (C.bind(self.fd, cast(bindAddr), ffi.sizeof(bindAddr)) < 0) then
            return nil, "bind failed: "..posix.getErrnoString()
        end

        if (C.listen(self.fd, 1) < 0) then
            return nil, "listen failed: "..posix.getErrnoString()
        end

        if (timeoutMs ~= nil) then
            local flags = C.fcntl(self.fd, F.GETFL)
            if (flags == -1) then
                return nil, "fcntl (F_GETFL) failed: "..posix.getErrnoString()
            end
            local newFlags = ffi.cast('int', bit.bor(flags, O.NONBLOCK))
            local ret = C.fcntl(self.fd, F.SETFL, newFlags)
            if (ret == -1) then
                return nil, "fcntl (F_SETFL) failed: "..posix.getErrnoString()
            end
        end

        local shouldWaitOnce = (timeoutMs ~= nil)
        local newFd

        for trial = 1, (shouldWaitOnce and 2 or 1) do
            if (trial == 1 and beforeFirstAccept ~= nil) then
                beforeFirstAccept()
            end

            newFd = C.accept(self.fd, nil, nil)

            if (newFd == -1) then
                if (shouldWaitOnce and trial == 1 and ffi.errno() == E.AGAIN) then
                    posix.poll({ events=POLL.IN, self.fd }, timeoutMs)
                else
                    return nil, "accept failed: "..posix.getErrnoString()
                end
            end
        end

        local ret = C.close(self.fd)
        -- If the file descriptor were invalid, we'd expect the functions above to fail.
        assert(ret >= 0)

        return posix.Fd(newFd)
    end,
}

-- Done!
return api
