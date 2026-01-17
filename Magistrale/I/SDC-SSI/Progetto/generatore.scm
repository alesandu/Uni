(set! %load-compiled-path (cons "./" %load-compiled-path))
(set! %load-compiled-path (cons "./libs/" %load-compiled-path))
(add-to-load-path "./")
(add-to-load-path "./libs/")

(use-modules
 (mtfa error-handler)
 (mtfa utils)
 (mtfa serializer)
 (mtfa unordered-set) ;;unordered set con chiavi (stringhe, numeri o sumimboli: tutto convertito in stringa). Persistente
 (mtfa unordered-map) ;;unordered map con cpolyhiavi (stringhe) e valori (qualsiasi cosa). persistente
 (mtfa star-map)      ;;Inse2drisce stringhe con o senza jolly, la stringa che definisce il jolly e il valore. Cerca le stringhe che matchano!
 (mtfa simple_db)
 (mtfa eis)
 ;;(mtfa fsm)
 (mtfa va)
 (mtfa extset)  ;;gestisce insiemi i cui elementi sono stringhe! consente operazioni di clone, set, check, get all.... Definisce una macro che consente di creare "al volo" una sottoclasse le cui istanze condividono gli stessi elementi.
 (mtfa umset)   ;;è una unordered map (non persistente) che ha stringhe come chiavi e ha insiemi di stringhe come valori. Ogni insert aggiunge all'insieme corrispondente. Definisce inoltre la mtfa-umap-list che consente di mappare liste come chiavi e qualsiasi valore come valore
 (mtfa web)
 (mtfa brg)
 (mtfa avl)
 (mtfa eqt)
 ;;
 (pfds sets)
 ;;
 (gnutls)
 ;;
 ;;La libreria guile lib
 ;;(logging logger)
 ;;(logging rotating-log)
 ;;(logging port-log)
 (scheme kwargs)
 (search basic)
 (math primes)
 (match-bind)
 (graph topological-sort)
 (debugging assert)
 ;;
 ;;i moduli di guile
 ;;((rnrs records syntactic) #:prefix rnrs::)
 (rnrs bytevectors)
 (rnrs arithmetic bitwise)
 ;; ((rnrs io ports)
 ;;  #:select (string->bytevector bytevector->string)
 ;;  #:prefix ioports::)
 ;;((rnrs) :version (6))
 ;;
 (srfi srfi-1)
 (srfi srfi-9)
 (srfi srfi-11)
 ((srfi srfi-18)
  #:prefix srfi-18::) ;;thread e mutex
 ;; date & time rinomina per avere un current time che non si sovrappone
 (srfi srfi-19)
 (srfi srfi-26)
 ;;(srfi srfi-28)
 (srfi srfi-41) ;;streams
 (srfi srfi-42) ;;Eager Comprehensions
 (srfi srfi-43)
 (srfi srfi-45)
 (srfi srfi-60)
 (srfi srfi-111)  ;;Boxes
 (srfi srfi-171)
 ;;
 (web uri)
 (system foreign)
 ;;
 (ice-9 format)
 (ice-9 ftw)
 (ice-9 rdelim)
 (ice-9 pretty-print)
 (ice-9 regex)
 (ice-9 iconv)
 (ice-9 string-fun)
 (ice-9 peg)
 (ice-9 peg string-peg)
 (ice-9 vlist)
 (ice-9 q)
 (ice-9 binary-ports)
 (ice-9 textual-ports)
 (ice-9 threads)
 (ice-9 hash-table)
 (ice-9 control)
 (ice-9 match)
 (ice-9 receive)
 (ice-9 eval-string)
 (ice-9 textual-ports)
 (ice-9 arrays)
 (ice-9 popen)
 (ice-9 exceptions)
 (ice-9 optargs)
 (ice-9 filesystem)
 ;;
 (oop goops)
 (oop goops describe)
 ;; (sxml simple)
 ;; (sxml ssax)
 ;; (sxml xpath)
 (json)
 (system syntax)
 (system foreign)
 ;;
 (web server)
 (web request)
 (web response)
 (web uri)
 ;;
 (web client)
 ;;
 (globals)
 )
;;Per esere sicuro di ricaricare le configurazioni nel caso siano variate
(config-force-reload)
;;
(define-syntax pk!
  (lambda (x)
    (syntax-case x ()
      ((_ a1)
       #'a1)
      ((_ a1 a2 ...)
       #'(pk! a2 ...)))))

(define-syntax string-compose
  (lambda (x)
    (syntax-case x ()
      ((_ a1)
       #'(with-output-to-string (lambda () (display a1))))
      ((_ a1 a2 ...)
       #'(string-append (with-output-to-string (lambda () (display a1)))
			(string-compose a2 ...))))))

;;è più lenta
;; (define-syntax string-composef
;;   (lambda (x)
;;     (syntax-case x ()
;;       ((_ a1)
;;        #'(format #f "~a" a1))
;;       ((_ a1 a2 ...)
;;        #'(string-append (format #f "~a" a1)
;; 			(string-composef a2 ...)))
;;       )))

(define (replace-with-values associations original-string)
  (if (null? associations)
      original-string
      (let ((caar-associations (caar associations))
	    (cdar-associations (cdar associations)))
	(cond
	 ((symbol? caar-associations) (set! caar-associations (symbol->string caar-associations)))
	 ((number? caar-associations) (set! caar-associations (number->string caar-associations))))
	(cond
	 ((symbol? cdar-associations) (set! cdar-associations (symbol->string cdar-associations)))
	 ((number? cdar-associations) (set! cdar-associations (number->string cdar-associations))))
	(replace-with-values (cdr associations)
			     (ReplaceAll original-string (string-append "\\$\\{" caar-associations "\\}") cdar-associations)))))
;;
(define-syntax-public string-repeater
  (lambda (x)
    (syntax-case x ()
      ((_ n ((var val update) ...) original-string)
       #'(let* ((var val) ...)
	   (let loop ((i 0) (lret '()))
	     (if (>= i n)
		 (string-concatenate-reverse/shared lret)
		 (begin
		   (set! lret (cons (replace-with-values `((var . ,var) ...) original-string) lret))
		   (begin (set! var update) ...)
		   (loop (1+ i) lret)))))))))

;;
;;CORE.YAML
(define (core-yaml)
  "# Copyright IBM Corp. All Rights Reserved.
#
# SPDX-License-Identifier: Apache-2.0
#

###############################################################################
#
#    Peer section
#
###############################################################################
peer:

    # The peer id provides a name for this peer instance and is used when
    # naming docker resources.
    id: jdoe

    # The networkId allows for logical separation of networks and is used when
    # naming docker resources.
    networkId: dev

    # The Address at local network interface this Peer will listen on.
    # By default, it will listen on all network interfaces
    listenAddress: 0.0.0.0:7051

    # The endpoint this peer uses to listen for inbound chaincode connections.
    # If this is commented-out, the listen address is selected to be
    # the peer's address (see below) with port 7052
    # chaincodeListenAddress: 0.0.0.0:7052

    # The endpoint the chaincode for this peer uses to connect to the peer.
    # If this is not specified, the chaincodeListenAddress address is selected.
    # And if chaincodeListenAddress is not specified, address is selected from
    # peer address (see below). If specified peer address is invalid then it
    # will fallback to the auto detected IP (local IP) regardless of the peer
    # addressAutoDetect value.
    # chaincodeAddress: 0.0.0.0:7052

    # When used as peer config, this represents the endpoint to other peers
    # in the same organization. For peers in other organization, see
    # gossip.externalEndpoint for more info.
    # When used as CLI config, this means the peer's endpoint to interact with
    address: 0.0.0.0:7051

    # Whether the Peer should programmatically determine its address
    # This case is useful for docker containers.
    # When set to true, will override peer address.
    addressAutoDetect: false

    # Keepalive settings for peer server and clients
    keepalive:
        # Interval is the duration after which if the server does not see
        # any activity from the client it pings the client to see if it's alive
        interval: 7200s
        # Timeout is the duration the server waits for a response
        # from the client after sending a ping before closing the connection
        timeout: 20s
        # MinInterval is the minimum permitted time between client pings.
        # If clients send pings more frequently, the peer server will
        # disconnect them
        minInterval: 60s
        # Client keepalive settings for communicating with other peer nodes
        client:
            # Interval is the time between pings to peer nodes.  This must
            # greater than or equal to the minInterval specified by peer
            # nodes
            interval: 60s
            # Timeout is the duration the client waits for a response from
            # peer nodes before closing the connection
            timeout: 20s
        # DeliveryClient keepalive settings for communication with ordering
        # nodes.
        deliveryClient:
            # Interval is the time between pings to ordering nodes.  This must
            # greater than or equal to the minInterval specified by ordering
            # nodes.
            interval: 60s
            # Timeout is the duration the client waits for a response from
            # ordering nodes before closing the connection
            timeout: 20s


    # Gossip related configuration
    gossip:
        # Bootstrap set to initialize gossip with.
        # This is a list of other peers that this peer reaches out to at startup.
        # Important: The endpoints here have to be endpoints of peers in the same
        # organization, because the peer would refuse connecting to these endpoints
        # unless they are in the same organization as the peer.
        bootstrap: 127.0.0.1:7051

        # NOTE: orgLeader and useLeaderElection parameters are mutual exclusive.
        # Setting both to true would result in the termination of the peer
        # since this is undefined state. If the peers are configured with
        # useLeaderElection=false, make sure there is at least 1 peer in the
        # organization that its orgLeader is set to true.

        # Defines whenever peer will initialize dynamic algorithm for
        # \"leader\" selection, where leader is the peer to establish
        # connection with ordering service and use delivery protocol
        # to pull ledger blocks from ordering service.
        useLeaderElection: false
        # Statically defines peer to be an organization \"leader\",
        # where this means that current peer will maintain connection
        # with ordering service and disseminate block across peers in
        # its own organization. Multiple peers or all peers in an organization
        # may be configured as org leaders, so that they all pull
        # blocks directly from ordering service.
        orgLeader: true

        # Interval for membershipTracker polling
        membershipTrackerInterval: 5s

        # Overrides the endpoint that the peer publishes to peers
        # in its organization. For peers in foreign organizations
        # see 'externalEndpoint'
        endpoint:
        # Maximum count of blocks stored in memory
        maxBlockCountToStore: 10
        # Max time between consecutive message pushes(unit: millisecond)
        maxPropagationBurstLatency: 10ms
        # Max number of messages stored until a push is triggered to remote peers
        maxPropagationBurstSize: 10
        # Number of times a message is pushed to remote peers
        propagateIterations: 1
        # Number of peers selected to push messages to
        propagatePeerNum: 3
        # Determines frequency of pull phases(unit: second)
        # Must be greater than digestWaitTime + responseWaitTime
        pullInterval: 4s
        # Number of peers to pull from
        pullPeerNum: 3
        # Determines frequency of pulling state info messages from peers(unit: second)
        requestStateInfoInterval: 4s
        # Determines frequency of pushing state info messages to peers(unit: second)
        publishStateInfoInterval: 4s
        # Maximum time a stateInfo message is kept until expired
        stateInfoRetentionInterval:
        # Time from startup certificates are included in Alive messages(unit: second)
        publishCertPeriod: 10s
        # Should we skip verifying block messages or not (currently not in use)
        skipBlockVerification: false
        # Dial timeout(unit: second)
        dialTimeout: 3s
        # Connection timeout(unit: second)
        connTimeout: 2s
        # Buffer size of received messages
        recvBuffSize: 20
        # Buffer size of sending messages
        sendBuffSize: 200
        # Time to wait before pull engine processes incoming digests (unit: second)
        # Should be slightly smaller than requestWaitTime
        digestWaitTime: 1s
        # Time to wait before pull engine removes incoming nonce (unit: milliseconds)
        # Should be slightly bigger than digestWaitTime
        requestWaitTime: 1500ms
        # Time to wait before pull engine ends pull (unit: second)
        responseWaitTime: 2s
        # Alive check interval(unit: second)
        aliveTimeInterval: 5s
        # Alive expiration timeout(unit: second)
        aliveExpirationTimeout: 25s
        # Reconnect interval(unit: second)
        reconnectInterval: 25s
        # Max number of attempts to connect to a peer
        maxConnectionAttempts: 120
        # Message expiration factor for alive messages
        msgExpirationFactor: 20
        # This is an endpoint that is published to peers outside of the organization.
        # If this isn't set, the peer will not be known to other organizations.
        externalEndpoint:
        # Leader election service configuration
        election:
            # Longest time peer waits for stable membership during leader election startup (unit: second)
            startupGracePeriod: 15s
            # Interval gossip membership samples to check its stability (unit: second)
            membershipSampleInterval: 1s
            # Time passes since last declaration message before peer decides to perform leader election (unit: second)
            leaderAliveThreshold: 10s
            # Time between peer sends propose message and declares itself as a leader (sends declaration message) (unit: second)
            leaderElectionDuration: 5s

        pvtData:
            # pullRetryThreshold determines the maximum duration of time private data corresponding for a given block
            # would be attempted to be pulled from peers until the block would be committed without the private data
            pullRetryThreshold: 60s
            # As private data enters the transient store, it is associated with the peer's ledger's height at that time.
            # transientstoreMaxBlockRetention defines the maximum difference between the current ledger's height upon commit,
            # and the private data residing inside the transient store that is guaranteed not to be purged.
            # Private data is purged from the transient store when blocks with sequences that are multiples
            # of transientstoreMaxBlockRetention are committed.
            transientstoreMaxBlockRetention: 1000
            # pushAckTimeout is the maximum time to wait for an acknowledgement from each peer
            # at private data push at endorsement time.
            pushAckTimeout: 3s
            # Block to live pulling margin, used as a buffer
            # to prevent peer from trying to pull private data
            # from peers that is soon to be purged in next N blocks.
            # This helps a newly joined peer catch up to current
            # blockchain height quicker.
            btlPullMargin: 10
            # the process of reconciliation is done in an endless loop, while in each iteration reconciler tries to
            # pull from the other peers the most recent missing blocks with a maximum batch size limitation.
            # reconcileBatchSize determines the maximum batch size of missing private data that will be reconciled in a
            # single iteration.
            reconcileBatchSize: 10
            # reconcileSleepInterval determines the time reconciler sleeps from end of an iteration until the beginning
            # of the next reconciliation iteration.
            reconcileSleepInterval: 1m
            # reconciliationEnabled is a flag that indicates whether private data reconciliation is enable or not.
            reconciliationEnabled: true
            # skipPullingInvalidTransactionsDuringCommit is a flag that indicates whether pulling of invalid
            # transaction's private data from other peers need to be skipped during the commit time and pulled
            # only through reconciler.
            skipPullingInvalidTransactionsDuringCommit: false
            # implicitCollectionDisseminationPolicy specifies the dissemination  policy for the peer's own implicit collection.
            # When a peer endorses a proposal that writes to its own implicit collection, below values override the default values
            # for disseminating private data.
            # Note that it is applicable to all channels the peer has joined. The implication is that requiredPeerCount has to
            # be smaller than the number of peers in a channel that has the lowest numbers of peers from the organization.
            implicitCollectionDisseminationPolicy:
               # requiredPeerCount defines the minimum number of eligible peers to which the peer must successfully
               # disseminate private data for its own implicit collection during endorsement. Default value is 0.
               requiredPeerCount: 0
               # maxPeerCount defines the maximum number of eligible peers to which the peer will attempt to
               # disseminate private data for its own implicit collection during endorsement. Default value is 1.
               maxPeerCount: 1

        # Gossip state transfer related configuration
        state:
            # indicates whenever state transfer is enabled or not
            # default value is true, i.e. state transfer is active
            # and takes care to sync up missing blocks allowing
            # lagging peer to catch up to speed with rest network
            enabled: false
            # checkInterval interval to check whether peer is lagging behind enough to
            # request blocks via state transfer from another peer.
            checkInterval: 10s
            # responseTimeout amount of time to wait for state transfer response from
            # other peers
            responseTimeout: 3s
            # batchSize the number of blocks to request via state transfer from another peer
            batchSize: 10
            # blockBufferSize reflects the size of the re-ordering buffer
            # which captures blocks and takes care to deliver them in order
            # down to the ledger layer. The actual buffer size is bounded between
            # 0 and 2*blockBufferSize, each channel maintains its own buffer
            blockBufferSize: 20
            # maxRetries maximum number of re-tries to ask
            # for single state transfer request
            maxRetries: 3

    # TLS Settings
    tls:
        # Require server-side TLS
        enabled:  false
        # Require client certificates / mutual TLS for inbound connections.
        # Note that clients that are not configured to use a certificate will
        # fail to connect to the peer.
        clientAuthRequired: false
        # X.509 certificate used for TLS server
        cert:
            file: tls/server.crt
        # Private key used for TLS server
        key:
            file: tls/server.key
        # rootcert.file represents the trusted root certificate chain used for verifying certificates
        # of other nodes during outbound connections.
        # It is not required to be set, but can be used to augment the set of TLS CA certificates
        # available from the MSPs of each channel’s configuration.
        rootcert:
            file: tls/ca.crt
        # If mutual TLS is enabled, clientRootCAs.files contains a list of additional root certificates
        # used for verifying certificates of client connections.
        # It augments the set of TLS CA certificates available from the MSPs of each channel’s configuration.
        # Minimally, set your organization's TLS CA root certificate so that the peer can receive join channel requests.
        clientRootCAs:
            files:
              - tls/ca.crt
        # Private key used for TLS when making client connections.
        # If not set, peer.tls.key.file will be used instead
        clientKey:
            file:
        # X.509 certificate used for TLS when making client connections.
        # If not set, peer.tls.cert.file will be used instead
        clientCert:
            file:

    # Authentication contains configuration parameters related to authenticating
    # client messages
    authentication:
        # the acceptable difference between the current server time and the
        # client's time as specified in a client request message
        timewindow: 15m

    # Path on the file system where peer will store data (eg ledger). This
    # location must be access control protected to prevent unintended
    # modification that might corrupt the peer operations.
    fileSystemPath: /var/hyperledger/production

    # BCCSP (Blockchain crypto provider): Select which crypto implementation or
    # library to use
    BCCSP:
        Default: SW
        # Settings for the SW crypto provider (i.e. when DEFAULT: SW)
        SW:
            # TODO: The default Hash and Security level needs refactoring to be
            # fully configurable. Changing these defaults requires coordination
            # SHA2 is hardcoded in several places, not only BCCSP
            Hash: SHA2
            Security: 256
            # Location of Key Store
            FileKeyStore:
                # If \"\", defaults to 'mspConfigPath'/keystore
                KeyStore:
        # Settings for the PKCS#11 crypto provider (i.e. when DEFAULT: PKCS11)
        PKCS11:
            # Location of the PKCS11 module library
            Library:
            # Token Label
            Label:
            # User PIN
            Pin:
            Hash:
            Security:

    # Path on the file system where peer will find MSP local configurations
    mspConfigPath: msp

    # Identifier of the local MSP
    # ----!!!!IMPORTANT!!!-!!!IMPORTANT!!!-!!!IMPORTANT!!!!----
    # Deployers need to change the value of the localMspId string.
    # In particular, the name of the local MSP ID of a peer needs
    # to match the name of one of the MSPs in each of the channel
    # that this peer is a member of. Otherwise this peer's messages
    # will not be identified as valid by other nodes.
    localMspId: SampleOrg

    # CLI common client config options
    client:
        # connection timeout
        connTimeout: 3s

    # Delivery service related config
    deliveryclient:
        # It sets the total time the delivery service may spend in reconnection
        # attempts until its retry logic gives up and returns an error
        reconnectTotalTimeThreshold: 3600s

        # It sets the delivery service <-> ordering service node connection timeout
        connTimeout: 3s

        # It sets the delivery service maximal delay between consecutive retries
        reConnectBackoffThreshold: 3600s

        # A list of orderer endpoint addresses which should be overridden
        # when found in channel configurations.
        addressOverrides:
        #  - from:
        #    to:
        #    caCertsFile:
        #  - from:
        #    to:
        #    caCertsFile:

    # Type for the local MSP - by default it's of type bccsp
    localMspType: bccsp

    # Used with Go profiling tools only in none production environment. In
    # production, it should be disabled (eg enabled: false)
    profile:
        enabled:     false
        listenAddress: 0.0.0.0:6060

    # Handlers defines custom handlers that can filter and mutate
    # objects passing within the peer, such as:
    #   Auth filter - reject or forward proposals from clients
    #   Decorators  - append or mutate the chaincode input passed to the chaincode
    #   Endorsers   - Custom signing over proposal response payload and its mutation
    # Valid handler definition contains:
    #   - A name which is a factory method name defined in
    #     core/handlers/library/library.go for statically compiled handlers
    #   - library path to shared object binary for pluggable filters
    # Auth filters and decorators are chained and executed in the order that
    # they are defined. For example:
    # authFilters:
    #   -
    #     name: FilterOne
    #     library: /opt/lib/filter.so
    #   -
    #     name: FilterTwo
    # decorators:
    #   -
    #     name: DecoratorOne
    #   -
    #     name: DecoratorTwo
    #     library: /opt/lib/decorator.so
    # Endorsers are configured as a map that its keys are the endorsement system chaincodes that are being overridden.
    # Below is an example that overrides the default ESCC and uses an endorsement plugin that has the same functionality
    # as the default ESCC.
    # If the 'library' property is missing, the name is used as the constructor method in the builtin library similar
    # to auth filters and decorators.
    # endorsers:
    #   escc:
    #     name: DefaultESCC
    #     library: /etc/hyperledger/fabric/plugin/escc.so
    handlers:
        authFilters:
          -
            name: DefaultAuth
          -
            name: ExpirationCheck    # This filter checks identity x509 certificate expiration
        decorators:
          -
            name: DefaultDecorator
        endorsers:
          escc:
            name: DefaultEndorsement
            library:
        validators:
          vscc:
            name: DefaultValidation
            library:

    #    library: /etc/hyperledger/fabric/plugin/escc.so
    # Number of goroutines that will execute transaction validation in parallel.
    # By default, the peer chooses the number of CPUs on the machine. Set this
    # variable to override that choice.
    # NOTE: overriding this value might negatively influence the performance of
    # the peer so please change this value only if you know what you're doing
    validatorPoolSize:

    # The discovery service is used by clients to query information about peers,
    # such as - which peers have joined a certain channel, what is the latest
    # channel config, and most importantly - given a chaincode and a channel,
    # what possible sets of peers satisfy the endorsement policy.
    discovery:
        enabled: true
        # Whether the authentication cache is enabled or not.
        authCacheEnabled: true
        # The maximum size of the cache, after which a purge takes place
        authCacheMaxSize: 1000
        # The proportion (0 to 1) of entries that remain in the cache after the cache is purged due to overpopulation
        authCachePurgeRetentionRatio: 0.75
        # Whether to allow non-admins to perform non channel scoped queries.
        # When this is false, it means that only peer admins can perform non channel scoped queries.
        orgMembersAllowedAccess: false

    # Limits is used to configure some internal resource limits.
    limits:
        # Concurrency limits the number of concurrently running requests to a service on each peer.
        # Currently this option is only applied to endorser service and deliver service.
        # When the property is missing or the value is 0, the concurrency limit is disabled for the service.
        concurrency:
            # endorserService limits concurrent requests to endorser service that handles chaincode deployment, query and invocation,
            # including both user chaincodes and system chaincodes.
            endorserService: 2500
            # deliverService limits concurrent event listeners registered to deliver service for blocks and transaction events.
            deliverService: 2500

###############################################################################
#
#    VM section
#
###############################################################################
vm:

    # Endpoint of the vm management system.  For docker can be one of the following in general
    # unix:///var/run/docker.sock
    # http://localhost:2375
    # https://localhost:2376
    endpoint: unix:///var/run/docker.sock

    # settings for docker vms
    docker:
        tls:
            enabled: false
            ca:
                file: docker/ca.crt
            cert:
                file: docker/tls.crt
            key:
                file: docker/tls.key

        # Enables/disables the standard out/err from chaincode containers for
        # debugging purposes
        attachStdout: false

        # Parameters on creating docker container.
        # Container may be efficiently created using ipam & dns-server for cluster
        # NetworkMode - sets the networking mode for the container. Supported
        # standard values are: `host`(default),`bridge`,`ipvlan`,`none`.
        # Dns - a list of DNS servers for the container to use.
        # Note:  `Privileged` `Binds` `Links` and `PortBindings` properties of
        # Docker Host Config are not supported and will not be used if set.
        # LogConfig - sets the logging driver (Type) and related options
        # (Config) for Docker. For more info,
        # https://docs.docker.com/engine/admin/logging/overview/
        # Note: Set LogConfig using Environment Variables is not supported.
        hostConfig:
            NetworkMode: host
            Dns:
               # - 192.168.0.1
            LogConfig:
                Type: json-file
                Config:
                    max-size: \"50m\"
                    max-file: \"5\"
            Memory: 2147483648

###############################################################################
#
#    Chaincode section
#
###############################################################################
chaincode:

    # The id is used by the Chaincode stub to register the executing Chaincode
    # ID with the Peer and is generally supplied through ENV variables
    # the `path` form of ID is provided when installing the chaincode.
    # The `name` is used for all other requests and can be any string.
    id:
        path:
        name:

    # Generic builder environment, suitable for most chaincode types
    builder: $(DOCKER_NS)/fabric-ccenv:$(TWO_DIGIT_VERSION)

    # Enables/disables force pulling of the base docker images (listed below)
    # during user chaincode instantiation.
    # Useful when using moving image tags (such as :latest)
    pull: false

    golang:
        # golang will never need more than baseos
        runtime: $(DOCKER_NS)/fabric-baseos:$(TWO_DIGIT_VERSION)

        # whether or not golang chaincode should be linked dynamically
        dynamicLink: false

    java:
        # This is an image based on java:openjdk-8 with addition compiler
        # tools added for java shim layer packaging.
        # This image is packed with shim layer libraries that are necessary
        # for Java chaincode runtime.
        runtime: $(DOCKER_NS)/fabric-javaenv:$(TWO_DIGIT_VERSION)

    node:
        # This is an image based on node:$(NODE_VER)-alpine
        runtime: $(DOCKER_NS)/fabric-nodeenv:$(TWO_DIGIT_VERSION)

    # List of directories to treat as external builders and launchers for
    # chaincode. The external builder detection processing will iterate over the
    # builders in the order specified below.
    externalBuilders: []
        # - path: /path/to/directory
        #   name: descriptive-builder-name
        #   propagateEnvironment:
        #      - ENVVAR_NAME_TO_PROPAGATE_FROM_PEER
        #      - GOPROXY

    # The maximum duration to wait for the chaincode build and install process
    # to complete.
    installTimeout: 300s

    # Timeout duration for starting up a container and waiting for Register
    # to come through.
    startuptimeout: 300s

    # Timeout duration for Invoke and Init calls to prevent runaway.
    # This timeout is used by all chaincodes in all the channels, including
    # system chaincodes.
    # Note that during Invoke, if the image is not available (e.g. being
    # cleaned up when in development environment), the peer will automatically
    # build the image, which might take more time. In production environment,
    # the chaincode image is unlikely to be deleted, so the timeout could be
    # reduced accordingly.
    executetimeout: 30s

    # There are 2 modes: \"dev\" and \"net\".
    # In dev mode, user runs the chaincode after starting peer from
    # command line on local machine.
    # In net mode, peer will run chaincode in a docker container.
    mode: net

    # keepalive in seconds. In situations where the communication goes through a
    # proxy that does not support keep-alive, this parameter will maintain connection
    # between peer and chaincode.
    # A value <= 0 turns keepalive off
    keepalive: 0

    # enabled system chaincodes
    system:
        _lifecycle: enable
        cscc: enable
        lscc: enable
        qscc: enable

    # Logging section for the chaincode container
    logging:
      # Default level for all loggers within the chaincode container
      level:  info
      # Override default level for the 'shim' logger
      shim:   warning
      # Format for the chaincode container logs
      format: '%{color}%{time:2006-01-02 15:04:05.000 MST} [%{module}] %{shortfunc} -> %{level:.4s} %{id:03x}%{color:reset} %{message}'

###############################################################################
#
#    Ledger section - ledger configuration encompasses both the blockchain
#    and the state
#
###############################################################################
ledger:

  blockchain:

  state:
    # stateDatabase - options are \"goleveldb\", \"CouchDB\"
    # goleveldb - default state database stored in goleveldb.
    # CouchDB - store state database in CouchDB
    stateDatabase: goleveldb
    # Limit on the number of records to return per query
    totalQueryLimit: 100000
    couchDBConfig:
       # It is recommended to run CouchDB on the same server as the peer, and
       # not map the CouchDB container port to a server port in docker-compose.
       # Otherwise proper security must be provided on the connection between
       # CouchDB client (on the peer) and server.
       couchDBAddress: 127.0.0.1:5984
       # This username must have read and write authority on CouchDB
       username:
       # The password is recommended to pass as an environment variable
       # during start up (eg CORE_LEDGER_STATE_COUCHDBCONFIG_PASSWORD).
       # If it is stored here, the file must be access control protected
       # to prevent unintended users from discovering the password.
       password:
       # Number of retries for CouchDB errors
       maxRetries: 3
       # Number of retries for CouchDB errors during peer startup.
       # The delay between retries doubles for each attempt.
       # Default of 10 retries results in 11 attempts over 2 minutes.
       maxRetriesOnStartup: 10
       # CouchDB request timeout (unit: duration, e.g. 20s)
       requestTimeout: 35s
       # Limit on the number of records per each CouchDB query
       # Note that chaincode queries are only bound by totalQueryLimit.
       # Internally the chaincode may execute multiple CouchDB queries,
       # each of size internalQueryLimit.
       internalQueryLimit: 1000
       # Limit on the number of records per CouchDB bulk update batch
       maxBatchUpdateSize: 1000
       # Warm indexes after every N blocks.
       # This option warms any indexes that have been
       # deployed to CouchDB after every N blocks.
       # A value of 1 will warm indexes after every block commit,
       # to ensure fast selector queries.
       # Increasing the value may improve write efficiency of peer and CouchDB,
       # but may degrade query response time.
       warmIndexesAfterNBlocks: 1
       # Create the _global_changes system database
       # This is optional.  Creating the global changes database will require
       # additional system resources to track changes and maintain the database
       createGlobalChangesDB: false
       # CacheSize denotes the maximum mega bytes (MB) to be allocated for the in-memory state
       # cache. Note that CacheSize needs to be a multiple of 32 MB. If it is not a multiple
       # of 32 MB, the peer would round the size to the next multiple of 32 MB.
       # To disable the cache, 0 MB needs to be assigned to the cacheSize.
       cacheSize: 64

  history:
    # enableHistoryDatabase - options are true or false
    # Indicates if the history of key updates should be stored.
    # All history 'index' will be stored in goleveldb, regardless if using
    # CouchDB or alternate database for the state.
    enableHistoryDatabase: true

  pvtdataStore:
    # the maximum db batch size for converting
    # the ineligible missing data entries to eligible missing data entries
    collElgProcMaxDbBatchSize: 5000
    # the minimum duration (in milliseconds) between writing
    # two consecutive db batches for converting the ineligible missing data entries to eligible missing data entries
    collElgProcDbBatchesInterval: 1000
    # The missing data entries are classified into two categories:
    # (1) prioritized
    # (2) deprioritized
    # Initially, all missing data are in the prioritized list. When the
    # reconciler is unable to fetch the missing data from other peers,
    # the unreconciled missing data would be moved to the deprioritized list.
    # The reconciler would retry deprioritized missing data after every
    # deprioritizedDataReconcilerInterval (unit: minutes). Note that the
    # interval needs to be greater than the reconcileSleepInterval
    deprioritizedDataReconcilerInterval: 60m

  snapshots:
    # Path on the file system where peer will store ledger snapshots
    rootDir: /var/hyperledger/production/snapshots

###############################################################################
#
#    Operations section
#
###############################################################################
operations:
    # host and port for the operations server
    listenAddress: 127.0.0.1:9443

    # TLS configuration for the operations endpoint
    tls:
        # TLS enabled
        enabled: false

        # path to PEM encoded server certificate for the operations server
        cert:
            file:

        # path to PEM encoded server key for the operations server
        key:
            file:

        # most operations service endpoints require client authentication when TLS
        # is enabled. clientAuthRequired requires client certificate authentication
        # at the TLS layer to access all resources.
        clientAuthRequired: false

        # paths to PEM encoded ca certificates to trust for client authentication
        clientRootCAs:
            files: []

###############################################################################
#
#    Metrics section
#
###############################################################################
metrics:
    # metrics provider is one of statsd, prometheus, or disabled
    provider: disabled

    # statsd configuration
    statsd:
        # network type: tcp or udp
        network: udp

        # statsd server address
        address: 127.0.0.1:8125

        # the interval at which locally cached counters and gauges are pushed
        # to statsd; timings are pushed immediately
        writeInterval: 10s

        # prefix is prepended to all emitted statsd metrics
        prefix:
")
;;
;;Costruisco la cartella "base" che contiene "peer-base.yaml" e "docker-compose-base.yaml"
;;considerato che non intendiamo modificare posizioni nel FS sdi fabric e che orderer è sempre orderer
;;l'unico parametro è il nome della rete
(define (peer-base-yaml network-name fabver)
  (replace-with-values
   `(("network-name" . ,network-name)
     ("fabver" . ,fabver)
     )
   "# Copyright IBM Corp. All Rights Reserved.
#
# SPDX-License-Identifier: Apache-2.0
#

version: '2'

services:
  peer-base:
    image: hyperledger/fabric-peer:${fabver}
    environment:
      - CORE_VM_ENDPOINT=unix:///host/var/run/docker.sock
      # the following setting starts chaincode containers on the same
      # bridge network as the peers
      # https://docs.docker.com/compose/networking/
      - CORE_VM_DOCKER_HOSTCONFIG_NETWORKMODE=${network-name}
      - GODEBUG=netdns=go
      - FABRIC_LOGGING_SPEC=INFO
      #- FABRIC_LOGGING_SPEC=DEBUG
      - CORE_PEER_TLS_ENABLED=true
      - CORE_PEER_GOSSIP_USELEADERELECTION=true
      - CORE_PEER_GOSSIP_ORGLEADER=false
      - CORE_PEER_PROFILE_ENABLED=true
      - CORE_PEER_TLS_CERT_FILE=/etc/hyperledger/fabric/tls/server.crt
      - CORE_PEER_TLS_KEY_FILE=/etc/hyperledger/fabric/tls/server.key
      - CORE_PEER_TLS_ROOTCERT_FILE=/etc/hyperledger/fabric/tls/ca.crt
      # Allow more time for chaincode container to build on install.
      - CORE_CHAINCODE_EXECUTETIMEOUT=300s
    working_dir: /opt/gopath/src/github.com/hyperledger/fabric/peer
    command: peer node start

  orderer-base:
    image: hyperledger/fabric-orderer:${fabver}
    environment:
      - FABRIC_LOGGING_SPEC=INFO
      - ORDERER_GENERAL_LISTENADDRESS=0.0.0.0
      - ORDERER_GENERAL_BOOTSTRAPMETHOD=file
      - ORDERER_GENERAL_BOOTSTRAPFILE=/var/hyperledger/orderer/orderer.genesis.block
      - ORDERER_GENERAL_LOCALMSPID=OrdererMSP
      - ORDERER_GENERAL_LOCALMSPDIR=/var/hyperledger/orderer/msp
      # enabled TLS
      - ORDERER_GENERAL_TLS_ENABLED=true
      - ORDERER_GENERAL_TLS_PRIVATEKEY=/var/hyperledger/orderer/tls/server.key
      - ORDERER_GENERAL_TLS_CERTIFICATE=/var/hyperledger/orderer/tls/server.crt
      - ORDERER_GENERAL_TLS_ROOTCAS=[/var/hyperledger/orderer/tls/ca.crt]
      - ORDERER_GENERAL_CLUSTER_CLIENTCERTIFICATE=/var/hyperledger/orderer/tls/server.crt
      - ORDERER_GENERAL_CLUSTER_CLIENTPRIVATEKEY=/var/hyperledger/orderer/tls/server.key
      - ORDERER_GENERAL_CLUSTER_ROOTCAS=[/var/hyperledger/orderer/tls/ca.crt]
    working_dir: /opt/gopath/src/github.com/hyperledger/fabric
    command: orderer

"))

;;Gli orderer continuano a chiamarsi orderer, al più cambierà il dominio di base
(define (docker-compose-base-yaml base-domain base-port num-of-orgs)
  "Generates the yaml for launching the blockchain. The peer/org ports starts from starting-port, ie 7050, and 
core peer address is 7051, chaincode address is 7052. Only one peer per org!
The next org will be 7061/7062, 7071.......7801, di 10 in 10.
The first orderer, by construction, is 7050
The first org, by construction, is 7051/7052
The second starts with 706, then 707, ....
"
  ;;Non cambio i nomi delle org sono tutti org[NNN]
  (let* ((header
	  (replace-with-values `((base-domain . ,base-domain) (base-port . ,base-port))
			       "# Copyright IBM Corp. All Rights Reserved.
#
# SPDX-License-Identifier: Apache-2.0
#

version: '2'

services:

  orderer.${base-domain}:
    container_name: orderer.${base-domain}
    extends:
      file: peer-base.yaml
      service: orderer-base
    volumes:
        - ../channel-artifacts/genesis.block:/var/hyperledger/orderer/orderer.genesis.block
        - ../crypto-config/ordererOrganizations/${base-domain}/orderers/orderer.${base-domain}/msp:/var/hyperledger/orderer/msp
        - ../crypto-config/ordererOrganizations/${base-domain}/orderers/orderer.${base-domain}/tls/:/var/hyperledger/orderer/tls
        - orderer.${base-domain}:/var/hyperledger/production/orderer
    ports:
      - ${base-port}:${base-port}

"))
	 (declaration "  peer${peer-number}.org${org-number}.${base-domain}:
    container_name: peer${peer-number}.org${org-number}.${base-domain}
    extends:
      file: peer-base.yaml
      service: peer-base
    environment:
      - CORE_PEER_ID=peer${peer-number}.org${org-number}.${base-domain}
      - CORE_PEER_ADDRESS=peer${peer-number}.org${org-number}.${base-domain}:${CORE_PEER_ADDRESS}
      - CORE_PEER_LISTENADDRESS=0.0.0.0:${CORE_PEER_LISTENADDRESS}
      - CORE_PEER_CHAINCODEADDRESS=peer${peer-number}.org${org-number}.${base-domain}:${CORE_PEER_CHAINCODEADDRESS}
      - CORE_PEER_CHAINCODELISTENADDRESS=0.0.0.0:${CORE_PEER_CHAINCODELISTENADDRESS}
      - CORE_PEER_GOSSIP_EXTERNALENDPOINT=peer${peer-number}.org${org-number}.${base-domain}:${CORE_PEER_GOSSIP_EXTERNALENDPOINT}
      - CORE_PEER_GOSSIP_BOOTSTRAP=peer${peer-number}.org${org-number}.${base-domain}:${CORE_PEER_GOSSIP_BOOTSTRAP}
      - CORE_PEER_LOCALMSPID=Org${org-number}MSP
    volumes:
        - /var/run/:/host/var/run/
        - ../crypto-config/peerOrganizations/org${org-number}.${base-domain}/peers/peer${peer-number}.org${org-number}.${base-domain}/msp:/etc/hyperledger/fabric/msp
        - ../crypto-config/peerOrganizations/org${org-number}.${base-domain}/peers/peer${peer-number}.org${org-number}.${base-domain}/tls:/etc/hyperledger/fabric/tls
        - peer${peer-number}.org${org-number}.${base-domain}:/var/hyperledger/production
    ports:
      - ${base-port}:${base-port}

")
	 (orgs (string-repeater num-of-orgs
				((current-base-address base-port (+ 100 current-base-address))
				 (base-domain base-domain base-domain)
				 (peer-number 0 peer-number)
				 (org-number 1 (1+ org-number))
				 (CORE_PEER_ADDRESS (+ 1 current-base-address) (+ 1 current-base-address) )
				 (CORE_PEER_LISTENADDRESS (+ 1 current-base-address) (+ 1 current-base-address) )
				 (CORE_PEER_CHAINCODEADDRESS (+ 2 current-base-address) (+ 2 current-base-address) )
				 (CORE_PEER_CHAINCODELISTENADDRESS (+ 2 current-base-address) (+ 2 current-base-address) )
				 (CORE_PEER_GOSSIP_BOOTSTRAP (+ 1 current-base-address) (+ 1 current-base-address) )
				 (CORE_PEER_GOSSIP_EXTERNALENDPOINT (+ 1 current-base-address) (+ 1 current-base-address) )
				 (base-port (+ 1 current-base-address) (+ 1 current-base-address)))
				declaration)))
    (string-concatenate `(,header ,orgs))))


(define (configtx-yaml base-domain base-port num-of-orgs)
  (let (
	(header "# Copyright IBM Corp. All Rights Reserved.
#
# SPDX-License-Identifier: Apache-2.0
#

---
################################################################################
#
#   Section: Organizations
#
#   - This section defines the different organizational identities which will
#   be referenced later in the configuration.
#
################################################################################
Organizations:

    # SampleOrg defines an MSP using the sampleconfig.  It should never be used
    # in production but may be used as a template for other definitions
    - &OrdererOrg
        # DefaultOrg defines the organization which is used in the sampleconfig
        # of the fabric.git development environment
        Name: OrdererOrg

        # ID to load the MSP definition as
        ID: OrdererMSP

        # MSPDir is the filesystem path which contains the MSP configuration
        MSPDir: crypto-config/ordererOrganizations/${base-domain}/msp

        # Policies defines the set of policies at this level of the config tree
        # For organization policies, their canonical path is usually
        #   /Channel/<Application|Orderer>/<OrgName>/<PolicyName>
        Policies:
            Readers:
                Type: Signature
                Rule: \"OR('OrdererMSP.member')\"
            Writers:
                Type: Signature
                Rule: \"OR('OrdererMSP.member')\"
            Admins:
                Type: Signature
                Rule: \"OR('OrdererMSP.admin')\"

")
	(simple-org "    - &Org${org-number}
        # DefaultOrg defines the organization which is used in the sampleconfig
        # of the fabric.git development environment
        Name: Org${org-number}MSP

        # ID to load the MSP definition as
        ID: Org${org-number}MSP

        MSPDir: crypto-config/peerOrganizations/org${org-number}.${base-domain}/msp

        # Policies defines the set of policies at this level of the config tree
        # For organization policies, their canonical path is usually
        #   /Channel/<Application|Orderer>/<OrgName>/<PolicyName>
        Policies:
            Readers:
                Type: Signature
                Rule: \"OR('Org${org-number}MSP.admin', 'Org${org-number}MSP.peer', 'Org${org-number}MSP.client')\"
            Writers:
                Type: Signature
                Rule: \"OR('Org${org-number}MSP.admin', 'Org${org-number}MSP.client')\"
            Admins:
                Type: Signature
                Rule: \"OR('Org${org-number}MSP.admin')\"
            Endorsement:
                Type: Signature
                Rule: \"OR('Org${org-number}MSP.peer')\"

        AnchorPeers:
            # AnchorPeers defines the location of peers which can be used
            # for cross org gossip communication.  Note, this value is only
            # encoded in the genesis block in the Application section context
            - Host: peer${peer-number}.org${org-number}.${base-domain}
              Port: ${base-port}

")
	(capabilities "################################################################################
#
#   SECTION: Capabilities
#
#   - This section defines the capabilities of fabric network. This is a new
#   concept as of v1.1.0 and should not be utilized in mixed networks with
#   v1.0.x peers and orderers.  Capabilities define features which must be
#   present in a fabric binary for that binary to safely participate in the
#   fabric network.  For instance, if a new MSP type is added, newer binaries
#   might recognize and validate the signatures from this type, while older
#   binaries without this support would be unable to validate those
#   transactions.  This could lead to different versions of the fabric binaries
#   having different world states.  Instead, defining a capability for a channel
#   informs those binaries without this capability that they must cease
#   processing transactions until they have been upgraded.  For v1.0.x if any
#   capabilities are defined (including a map with all capabilities turned off)
#   then the v1.0.x peer will deliberately crash.
#
################################################################################
Capabilities:
    # Channel capabilities apply to both the orderers and the peers and must be
    # supported by both.
    # Set the value of the capability to true to require it.
    Channel: &ChannelCapabilities
        # V2_0 capability ensures that orderers and peers behave according
        # to v2.0 channel capabilities. Orderers and peers from
        # prior releases would behave in an incompatible way, and are therefore
        # not able to participate in channels at v2.0 capability.
        # Prior to enabling V2.0 channel capabilities, ensure that all
        # orderers and peers on a channel are at v2.0.0 or later.
        V2_0: true

    # Orderer capabilities apply only to the orderers, and may be safely
    # used with prior release peers.
    # Set the value of the capability to true to require it.
    Orderer: &OrdererCapabilities
        # V2_0 orderer capability ensures that orderers behave according
        # to v2.0 orderer capabilities. Orderers from
        # prior releases would behave in an incompatible way, and are therefore
        # not able to participate in channels at v2.0 orderer capability.
        # Prior to enabling V2.0 orderer capabilities, ensure that all
        # orderers on channel are at v2.0.0 or later.
        V2_0: true

    # Application capabilities apply only to the peer network, and may be safely
    # used with prior release orderers.
    # Set the value of the capability to true to require it.
    Application: &ApplicationCapabilities
        # V2_0 application capability ensures that peers behave according
        # to v2.0 application capabilities. Peers from
        # prior releases would behave in an incompatible way, and are therefore
        # not able to participate in channels at v2.0 application capability.
        # Prior to enabling V2.0 application capabilities, ensure that all
        # peers on channel are at v2.0.0 or later.
        V2_0: true

")
	(application "################################################################################
#
#   SECTION: Application
#
#   - This section defines the values to encode into a config transaction or
#   genesis block for application related parameters
#
################################################################################
Application: &ApplicationDefaults

    # Organizations is the list of orgs which are defined as participants on
    # the application side of the network
    Organizations:

    # Policies defines the set of policies at this level of the config tree
    # For Application policies, their canonical path is
    #   /Channel/Application/<PolicyName>
    Policies:
        Readers:
            Type: ImplicitMeta
            Rule: \"ANY Readers\"
        Writers:
            Type: ImplicitMeta
            Rule: \"ANY Writers\"
        Admins:
            Type: ImplicitMeta
            Rule: \"MAJORITY Admins\"
        LifecycleEndorsement:
            Type: ImplicitMeta
            Rule: \"MAJORITY Endorsement\"
        Endorsement:
            Type: ImplicitMeta
            Rule: \"MAJORITY Endorsement\"

    Capabilities:
        <<: *ApplicationCapabilities

")
	(orderer "################################################################################
#
#   SECTION: Orderer
#
#   - This section defines the values to encode into a config transaction or
#   genesis block for orderer related parameters
#
################################################################################
Orderer: &OrdererDefaults

    # Orderer Type: The orderer implementation to start
    OrdererType: etcdraft

    Addresses:
        - orderer.${base-domain}:${base-port}

    # Batch Timeout: The amount of time to wait before creating a batch
    BatchTimeout: 1ms

    # Batch Size: Controls the number of messages batched into a block
    BatchSize:

        # Max Message Count: The maximum number of messages to permit in a batch
        MaxMessageCount: 2

        # Absolute Max Bytes: The absolute maximum number of bytes allowed for
        # the serialized messages in a batch.
        AbsoluteMaxBytes: 99 MB

        # Preferred Max Bytes: The preferred maximum number of bytes allowed for
        # the serialized messages in a batch. A message larger than the preferred
        # max bytes will result in a batch larger than preferred max bytes.
        PreferredMaxBytes: 4 MB

    # Organizations is the list of orgs which are defined as participants on
    # the orderer side of the network
    Organizations:

    # Policies defines the set of policies at this level of the config tree
    # For Orderer policies, their canonical path is
    #   /Channel/Orderer/<PolicyName>
    Policies:
        Readers:
            Type: ImplicitMeta
            Rule: \"ANY Readers\"
        Writers:
            Type: ImplicitMeta
            Rule: \"ANY Writers\"
        Admins:
            Type: ImplicitMeta
            Rule: \"MAJORITY Admins\"
        # BlockValidation specifies what signatures must be included in the block
        # from the orderer for the peer to validate it.
        BlockValidation:
            Type: ImplicitMeta
            Rule: \"ANY Writers\"

")
	(channel "
################################################################################
#
#   CHANNEL
#
#   This section defines the values to encode into a config transaction or
#   genesis block for channel related parameters.
#
################################################################################
Channel: &ChannelDefaults
    # Policies defines the set of policies at this level of the config tree
    # For Channel policies, their canonical path is
    #   /Channel/<PolicyName>
    Policies:
        # Who may invoke the 'Deliver' API
        Readers:
            Type: ImplicitMeta
            Rule: \"ANY Readers\"
        # Who may invoke the 'Broadcast' API
        Writers:
            Type: ImplicitMeta
            Rule: \"ANY Writers\"
        # By default, who may modify elements at this config level
        Admins:
            Type: ImplicitMeta
            Rule: \"MAJORITY Admins\"

    # Capabilities describes the channel level capabilities, see the
    # dedicated Capabilities section elsewhere in this file for a full
    # description
    Capabilities:
        <<: *ChannelCapabilities

")
	(profiles (string-append
		   "################################################################################
#
#   Profile
#
#   - Different configuration profiles may be encoded here to be specified
#   as parameters to the configtxgen tool
#
################################################################################
Profiles:

    TwoOrgsOrdererGenesis:
        <<: *ChannelDefaults
        Orderer:
            <<: *OrdererDefaults
            Organizations:
                - *OrdererOrg
            Capabilities:
                <<: *OrdererCapabilities
        Consortiums:
            SampleConsortium:
                Organizations:
"
		   (string-repeater num-of-orgs ((org-number 1 (1+ org-number))) "                    - *Org${org-number}
")
		   "    TwoOrgsChannel:
        Consortium: SampleConsortium
        <<: *ChannelDefaults
        Application:
            <<: *ApplicationDefaults
            Organizations:
"
		   (string-repeater num-of-orgs ((org-number 1 (1+ org-number))) "                - *Org${org-number}
")
		   "            Capabilities:
                <<: *ApplicationCapabilities

    SampleMultiNodeEtcdRaft:
        <<: *ChannelDefaults
        Capabilities:
            <<: *ChannelCapabilities
        Orderer:
            <<: *OrdererDefaults
            OrdererType: etcdraft
            EtcdRaft:
                Consenters:
"
		   (string-repeater num-of-orgs ((count 1 (1+ count))
						 (ord-num "" count)
						 (base-domain base-domain base-domain)
						 (PORT base-port (+ 100 PORT)))
				    "                - Host: orderer${ord-num}.${base-domain}
                  Port: ${PORT}
                  ClientTLSCert: crypto-config/ordererOrganizations/${base-domain}/orderers/orderer${ord-num}.${base-domain}/tls/server.crt
                  ServerTLSCert: crypto-config/ordererOrganizations/${base-domain}/orderers/orderer${ord-num}.${base-domain}/tls/server.crt
")
		   "
            Addresses:
"
		   (string-repeater num-of-orgs ((count 1 (1+ count))
						 (ord-num "" count)
						 (base-domain base-domain base-domain)
						 (PORT base-port (+ 100 PORT))
						 )
				    "                - orderer${ord-num}.${base-domain}:${PORT}
")
		   "
            Organizations:
            - *OrdererOrg
            Capabilities:
                <<: *OrdererCapabilities
        Application:
            <<: *ApplicationDefaults
            Organizations:
            - <<: *OrdererOrg
        Consortiums:
            SampleConsortium:
                Organizations:
"
		   (string-repeater num-of-orgs ((org-number 1 (1+ org-number))) "                - *Org${org-number}
")
		   ))
	)
    ;;
    ;;Concateniamo e generiamo
    (string-append
     (replace-with-values `((base-domain . ,base-domain)) header)
     ;;
     (string-repeater num-of-orgs ((org-number 1 (1+ org-number))
				   (peer-number 0 peer-number)
				   (base-domain base-domain base-domain)
				   (base-port (1+ base-port) (+ 100 base-port)))
		      simple-org)
     ;;
     capabilities
     ;;
     application
     ;;
     (replace-with-values `((base-domain . ,base-domain)
			    (base-port . ,base-port))
			  orderer)
     ;;
     channel
     ;;
     profiles
     )))


(define (crypto-config-yaml num-of-orgs base-domain num-of-users)
  (string-append
   (replace-with-values `((base-domain . ,base-domain))
			"# Copyright IBM Corp. All Rights Reserved.
#
# SPDX-License-Identifier: Apache-2.0
#

# ---------------------------------------------------------------------------
# \"OrdererOrgs\" - Definition of organizations managing orderer nodes
# ---------------------------------------------------------------------------
OrdererOrgs:
  # ---------------------------------------------------------------------------
  # Orderer
  # ---------------------------------------------------------------------------
  - Name: Orderer
    Domain: ${base-domain}
    # ---------------------------------------------------------------------------
    # \"Specs\" - See PeerOrgs below for complete description
    # ---------------------------------------------------------------------------
    Specs:
")
   (string-repeater num-of-orgs ((count 1 (1+ count))
				 (dom-number "" count))
		    "      - Hostname: orderer${dom-number}
")
   "
PeerOrgs:
"
  (string-repeater num-of-orgs ((count 1 (1+ count))
				(dom-number "" count)
				(org-number count count)
				(base-domain base-domain base-domain)
				(num-of-users num-of-users num-of-users))
		   "  - Name: Org${org-number}
    Domain: org${org-number}.${base-domain}
    EnableNodeOUs: true
    Template:
      Count: 1
    Users:
      Count: ${num-of-users}
")
  "
"))

;;Con questo genero gli up e i down per ogni org
(define (generate-host-up num-of-orgs network-name base-domain base-port)
  (for-each (lambda (org-num)
	    (let ((twodigits (format #f "~d" org-num)))
	      (fs-io-from-string (string-append "host" twodigits "up.sh")
				 (format #f "docker compose -f host~a.yaml up -d" twodigits))
	      (chmod (string-append "host" twodigits "up.sh") #o777)
	      ))
	    (iota num-of-orgs 1)))
;;
(define (generate-host-down num-of-orgs network-name base-domain base-port)
  (for-each (lambda (org-num)
	     (let ((twodigits (format #f "~d" org-num)))
	       (fs-io-from-string (string-append "host" twodigits "down.sh")
				  (format #f "docker compose -f host~a.yaml down $*" twodigits))
	       (chmod (string-append "host" twodigits "down.sh") #o777)
	       ))
	    (iota num-of-orgs 1)))
;;
(define (generate-host-yaml num-of-orgs network-name base-domain base-port systemchannel fabver)
  (for-each (lambda (org-num)
	      (let ((twodigits (format #f "~d" org-num))
		    (host-1 "# Copyright IBM Corp. All Rights Reserved.
#
# SPDX-License-Identifier: Apache-2.0
#

version: '2'

volumes:
  orderer.${base-domain}:
  peer0.org${org-num}.${base-domain}:

networks:
  byfn:
    external:
      name: ${network-name}

services:

  orderer.${base-domain}:
    extends:
      file:   base/docker-compose-base.yaml
      service: orderer.${base-domain}
    container_name: orderer.${base-domain}
    networks:
      - byfn

  peer0.org${org-num}.${base-domain}:
    container_name: peer0.org${org-num}.${base-domain}
    extends:
      file:  base/docker-compose-base.yaml
      service: peer0.org${org-num}.${base-domain}
    networks:
      - byfn

  cli:
    container_name: cli
    image: hyperledger/fabric-tools:${fabver}
    tty: true
    stdin_open: true
    environment:
      - SYS_CHANNEL=${systemchannel}
      - GOPATH=/opt/gopath
      - CORE_VM_ENDPOINT=unix:///host/var/run/docker.sock
      #- FABRIC_LOGGING_SPEC=DEBUG
      - FABRIC_LOGGING_SPEC=INFO
      - CORE_PEER_ID=cli
      - CORE_PEER_ADDRESS=peer0.org${org-num}.${base-domain}:${base-port}
      - CORE_PEER_LOCALMSPID=Org${org-num}MSP
      - CORE_PEER_TLS_ENABLED=true
      - CORE_PEER_TLS_CERT_FILE=/opt/gopath/src/github.com/hyperledger/fabric/peer/crypto/peerOrganizations/org${org-num}.${base-domain}/peers/peer0.org${org-num}.${base-domain}/tls/server.crt
      - CORE_PEER_TLS_KEY_FILE=/opt/gopath/src/github.com/hyperledger/fabric/peer/crypto/peerOrganizations/org${org-num}.${base-domain}/peers/peer0.org${org-num}.${base-domain}/tls/server.key
      - CORE_PEER_TLS_ROOTCERT_FILE=/opt/gopath/src/github.com/hyperledger/fabric/peer/crypto/peerOrganizations/org${org-num}.${base-domain}/peers/peer0.org${org-num}.${base-domain}/tls/ca.crt
      - CORE_PEER_MSPCONFIGPATH=/opt/gopath/src/github.com/hyperledger/fabric/peer/crypto/peerOrganizations/org${org-num}.${base-domain}/users/Admin@org${org-num}.${base-domain}/msp
    working_dir: /opt/gopath/src/github.com/hyperledger/fabric/peer
    command: /bin/bash
    volumes:
        - /var/run/:/host/var/run/
        - ./chaincode/:/opt/gopath/src/github.com/chaincode
        - ./crypto-config:/opt/gopath/src/github.com/hyperledger/fabric/peer/crypto/
        - ./scripts:/opt/gopath/src/github.com/hyperledger/fabric/peer/scripts/
        - ./channel-artifacts:/opt/gopath/src/github.com/hyperledger/fabric/peer/channel-artifacts
    depends_on:
      - orderer.${base-domain}
      - peer0.org${org-num}.${base-domain}
    networks:
      - byfn

")
		    (host-n "# Copyright IBM Corp. All Rights Reserved.
#
# SPDX-License-Identifier: Apache-2.0
#

version: '2'

volumes:
  orderer${ord-num}.${base-domain}:
  peer0.org${org-num}.${base-domain}:

networks:
  byfn:
    external:
      name: ${network-name}

services:

  orderer${ord-num}.${base-domain}:
    extends:
      file: base/peer-base.yaml
      service: orderer-base
    environment:
      - ORDERER_GENERAL_LISTENPORT=${base-port}
    container_name: orderer${ord-num}.${base-domain}
    networks:
    - byfn
    volumes:
        - ./channel-artifacts/genesis.block:/var/hyperledger/orderer/orderer.genesis.block
        - ./crypto-config/ordererOrganizations/${base-domain}/orderers/orderer${ord-num}.${base-domain}/msp:/var/hyperledger/orderer/msp
        - ./crypto-config/ordererOrganizations/${base-domain}/orderers/orderer${ord-num}.${base-domain}/tls/:/var/hyperledger/orderer/tls
        - orderer${ord-num}.${base-domain}:/var/hyperledger/production/orderer
    ports:
    - ${base-port}:${base-port}

  peer0.org${org-num}.${base-domain}:
    container_name: peer0.org${org-num}.${base-domain}
    extends:
      file:  base/docker-compose-base.yaml
      service: peer0.org${org-num}.${base-domain}
    networks:
      - byfn
"))
		(if (= 1 org-num)
		    (fs-io-from-string (string-append "host" twodigits ".yaml")
				       (replace-with-values `((base-domain . ,base-domain)
							      (org-num . ,org-num)
							      (network-name . ,network-name)
							      (fabver . ,fabver)
							      (systemchannel . ,systemchannel)
							      (base-port . ,(1+ base-port)))
							    host-1
							    ))
		    (fs-io-from-string (string-append "host" twodigits ".yaml")
				       (replace-with-values `((base-domain . ,base-domain)
							      (org-num . ,org-num)
							      (network-name . ,network-name)
							      (base-port . ,(+ (* 100 (1- org-num)) base-port))
							      (ord-num . ,(if (= 1 org-num) "" org-num))
							      )
							    host-n
							    )))))
	      (iota num-of-orgs 1)))
;;
;;;;Run External Program inside a pipeline
(define (RunExtProg commands)
  "Example: (RunExtProg '((\"ls\" \"generatore.json\")(\"wc\")))"
  (let ((success? (lambda (pid) (zero? (status:exit-val (cdr (waitpid pid)))))))
    (receive (from to pids) (pipeline commands)
      (let* ((data-read (with-input-from-port
			    from (lambda ()
				   (let loop ((input (read-line)))
				     (if (eof-object? input)
					 '()
					 (cons input (loop (read-line))))))))
	     (index (list-index (negate success?) (reverse pids))))
	(close to)
	(close from)
	(if (not index)
	    data-read
	    (begin
	      (string-append "pipeline failed in command: "
			     (string-join (list-ref commands index)))
	      '()))))))

(define (generate-make-certs-and-genesis)
  (with-output-to-file "make-cert-and-genesis.sh"
		    (lambda ()
		      (let ((result "")
			    (fabric-tools (string-compose "docker run -v \"$PWD\":/this hyperledger/fabric-tools:" cfg::fabver " /bin/bash -c ")))
			;;
			;;cancello crypto-config e channel-artifacts
			(display "/bin/rm -rf crypto-config") (newline)
			(display "mkdir crypto-config") (newline)
			(display "/bin/rm -rf channel-artifacts") (newline)
			(display "mkdir channel-artifacts") (newline)
			;;
			;;Genera crypto-config
			(display (replace-with-values `((fabric-tools . ,fabric-tools))
						      "${fabric-tools} \"cd /this; cryptogen generate --config=crypto-config.yaml --output=crypto-config\"\n"))
			;;ora gli artifacts
			(display (replace-with-values `((systemchannel . ,cfg::systemchannel) (fabric-tools . ,fabric-tools))
						      "${fabric-tools} \"cd /this; configtxgen -configPath /this/ -profile SampleMultiNodeEtcdRaft -channelID ${systemchannel} -outputBlock ./channel-artifacts/genesis.block\"\n"))
			;;
			(display (replace-with-values `((channelid . ,cfg::channelid) (fabric-tools . ,fabric-tools))
						      "${fabric-tools} \"cd /this; configtxgen -configPath /this/ -profile TwoOrgsChannel -outputCreateChannelTx ./channel-artifacts/${channelid}.tx -channelID ${channelid}\"\n"))
			;;
			(do ((i 1 (1+ i))) ((> i cfg::num-of-orgs))
			  (display (replace-with-values `((channelid . ,cfg::channelid) (org-num . ,i) (fabric-tools . ,fabric-tools))
							"${fabric-tools} \"cd /this; configtxgen -configPath /this/ -profile TwoOrgsChannel -outputAnchorPeersUpdate ./channel-artifacts/Org${org-num}MSPanchors.tx -channelID ${channelid} -asOrg Org${org-num}MSP\"\n")))
			;;
			;;Dqato che ho generato le cose con docker, sono diventate di root. Le cambio!
			(display "sudo chown -R $USER:$USER crypto-config channel-artifacts\n")
			;;Qui posso chiamare le ccp-generate ma voglio che siano in crypto-config
			;;quindi le sposto dalla cartella di lavoro in crypto-config
			(display "/bin/cp -f ccp-generate.sh  crypto-config/\n")
			(display "/bin/cp -f ccp-template.json  crypto-config/\n")
			(display "/bin/cp -f ccp-template.yaml  crypto-config/\n")
			;;E qui mando in esecuzione cpp-generate.json
			(display "chmod a+x crypto-config/ccp-generate.sh\n")
			(display "./crypto-config/ccp-generate.sh\n")
			)))
  (chmod "make-cert-and-genesis.sh" #o777)
  )
;;
;;La generazione dei file di blockchain explorer
(define (generate-bc-explorer-files)
  (try-catch #f
  (let ((docker-compose.yaml
	   (replace-with-values `((network . ,cfg::network-name))
				"# SPDX-License-Identifier: Apache-2.0
version: '2.1'

volumes:
  pgdata:
  walletstore:

networks:
  mynetwork.com:
    external:
      name: ${network}

services:

  explorerdb.mynetwork.com:
    image: hyperledger/explorer-db:latest
    container_name: explorerdb.mynetwork.com
    hostname: explorerdb.mynetwork.com
    environment:
      - DATABASE_DATABASE=fabricexplorer
      - DATABASE_USERNAME=hppoc
      - DATABASE_PASSWORD=password
    healthcheck:
      test: \"pg_isready -h localhost -p 5432 -q -U postgres\"
      interval: 30s
      timeout: 10s
      retries: 5
    volumes:
      - pgdata:/var/lib/postgresql/data
    networks:
      - mynetwork.com

  explorer.mynetwork.com:
    image: hyperledger/explorer:latest
    container_name: explorer.mynetwork.com
    hostname: explorer.mynetwork.com
    environment:
      - DATABASE_HOST=explorerdb.mynetwork.com
      - DATABASE_DATABASE=fabricexplorer
      - DATABASE_USERNAME=hppoc
      - DATABASE_PASSWD=password
      - LOG_LEVEL_APP=debug
      - LOG_LEVEL_DB=debug
      - LOG_LEVEL_CONSOLE=info
      - LOG_CONSOLE_STDOUT=true
      - DISCOVERY_AS_LOCALHOST=false
    volumes:
      - ./config.json:/opt/explorer/app/platform/fabric/config.json
      - ./connection-profile:/opt/explorer/app/platform/fabric/connection-profile
      - ../crypto-config:/tmp/crypto
      - walletstore:/opt/explorer/wallet
    ports:
      - 8080:8080
    depends_on:
      explorerdb.mynetwork.com:
        condition: service_healthy
    networks:
      - mynetwork.com
"))
	(test-network.json
	 (replace-with-values `((network . ,cfg::network-name)
				(channel . ,cfg::channelid)
				(domain . ,cfg::base-domain)
				(port . ,(+ 1 cfg::base-port))
				)
			      "{
	\"name\" : \"test-network\",
	\"version\" : \"1.0.0\",
	\"client\" : {
		\"tlsEnable\" : true,
		\"adminCredential\" : {
			\"id\" : \"exploreradmin\",
			\"password\" : \"exploreradminpw\"
		},
		\"enableAuthentication\" : true,
		\"organization\" : \"Org1MSP\",
		\"connection\" : {
			\"timeout\" : {
				\"peer\" : {
					\"endorser\" : \"300\"
				},
				\"orderer\" : \"300\"
			}
		}
	},
	\"channels\" : {
		\"${channel}\" : {
			\"peers\" : {
				\"peer0.org1.${domain}\" : { }
			}
		}
	},
	\"organizations\" : {
		\"Org1MSP\" : {
			\"mspid\" : \"Org1MSP\",
			\"adminPrivateKey\" : {
				\"path\" : \"/tmp/crypto/peerOrganizations/org1.${domain}/users/Admin@org1.${domain}/msp/keystore/priv_sk\"
			},
			\"peers\" : [
				\"peer0.org1.${domain}\"
				],
			\"signedCert\" : {
				\"path\" : \"/tmp/crypto/peerOrganizations/org1.${domain}/users/Admin@org1.${domain}/msp/signcerts/Admin@org1.${domain}-cert.pem\"
			}
		}
	},
	\"peers\" : {
		\"peer0.org1.${domain}\" : {
			\"tlsCACerts\" : {
				\"path\" : \"/tmp/crypto/peerOrganizations/org1.${domain}/peers/peer0.org1.${domain}/tls/ca.crt\"
			},
			\"url\" : \"grpcs://peer0.org1.${domain}:${port}\"
		}
	}
}
"
))
	(config.json (replace-with-values `((network . ,cfg::network-name)
				(channel . ,cfg::channelid)
				(domain . ,cfg::base-domain)
				(port . ,(+ 1 cfg::base-port))
				)"
{
	\"network-configs\": {
		\"test-network\": {
			\"name\": \"${network}\",
			\"profile\": \"./connection-profile/test-network.json\"
		}
	},
	\"license\": \"Apache-2.0\"
}
")))
    ;; L'obiettivo è avere nella cartella blockchain-explorer i seguenti files
    ;; docker-compose.yaml
    ;; config.json
    ;; connection-profile/test-network.json
    ;; organizations/ordererOrganizations/
    ;; organizations/peerOrganizations/
    ;;i primi tre li genero qui, gli altri sono già presenti se non viene alterata la struttura di folder!!!
    (fs-io-from-string (string-append "blockchain-explorer/" "docker-compose.yaml") docker-compose.yaml)
    (fs-io-from-string (string-append "blockchain-explorer/" "config.json") config.json)
    (make-directories (string-append "blockchain-explorer/" "connection-profile"))
    (fs-io-from-string (string-append "blockchain-explorer/" "connection-profile/" "test-network.json") test-network.json)
    )
  ))
;;
(define-syntax myassert
  (lambda (x)
    (syntax-case x ()
      ((_ val msg anon)
       #'(let ((v val))
	   (unless v
	     (cerr msg)
	     (error))
	   (anon v))))))
;;
;;Genera ccp-generate.sh, ccp-template.json, ccp-template.yaml
(define (generate-sh-json-yaml)
  (let ((ccp-generate.sh (string-append
			  "
#!/bin/bash

function one_line_pem {
    echo \"`awk 'NF {sub(/\\\\n/, \"\"); printf \"%s\\\\\\\\\\\\\\n\",$0;}' $1`\"
}

function json_ccp {
    local PP=$(one_line_pem $4)
    local CP=$(one_line_pem $5)
    sed -e \"s/\\${ORG}/$1/\" \\
        -e \"s/\\${P0PORT}/$2/\" \\
        -e \"s/\\${CAPORT}/$3/\" \\
        -e \"s#\\${PEERPEM}#$PP#\" \\
        -e \"s#\\${CAPEM}#$CP#\" \\
        crypto-config/ccp-template.json
}
			  
function yaml_ccp {
    local PP=$(one_line_pem $4)
    local CP=$(one_line_pem $5)
    sed -e \"s/\\${ORG}/$1/\" \\
        -e \"s/\\${P0PORT}/$2/\" \\
        -e \"s/\\${CAPORT}/$3/\" \\
        -e \"s#\\${PEERPEM}#$PP#\" \\
        -e \"s#\\${CAPEM}#$CP#\" \\
        crypto-config/ccp-template.yaml | sed -e $'s/\\\\\\\\n/\\\\\\n          /g'
}

"
			  (string-repeater cfg::num-of-orgs
					   ((org-num 1 (1+ org-num))
					    (peer0-port (1+ cfg::base-port) (+ 100 peer0-port))
					    (ca-port (+ 4 cfg::base-port) (+ 100 ca-port)) ;;non utilizzata!
					    (domain cfg::base-domain domain)
					    )
					   "
ORG=${org-num}
P0PORT=${peer0-port}
CAPORT=${ca-port}
PEERPEM=crypto-config/peerOrganizations/org${org-num}.${domain}/tlsca/tlsca.org${org-num}.${domain}-cert.pem
CAPEM=crypto-config/peerOrganizations/org${org-num}.${domain}/ca/ca.org${org-num}.${domain}-cert.pem

echo \"$(json_ccp $ORG $P0PORT $CAPORT $PEERPEM $CAPEM)\" > crypto-config/peerOrganizations/org${org-num}.${domain}/connection-org${org-num}.json
echo \"$(yaml_ccp $ORG $P0PORT $CAPORT $PEERPEM $CAPEM)\" > crypto-config/peerOrganizations/org${org-num}.${domain}/connection-org${org-num}.yaml

")))
	(ccp-template.json (replace-with-values `((domain . ,cfg::base-domain))
						
						"						{
    \"name\": \"test-network-org${ORG}\",
    \"version\": \"1.0.0\",
    \"client\": {
        \"organization\": \"Org${ORG}\",
        \"connection\": {
            \"timeout\": {
                \"peer\": {
                    \"endorser\": \"300\"
                }
            }
        }
    },
    \"organizations\": {
        \"Org${ORG}\": {
            \"mspid\": \"Org${ORG}MSP\",
            \"peers\": [
                \"peer0.org${ORG}.${domain}\"
            ],
            \"certificateAuthorities\": [
                \"ca.org${ORG}.${domain}\"
            ]
        }
    },
    \"peers\": {
        \"peer0.org${ORG}.${domain}\": {
            \"url\": \"grpcs://peer0.org${ORG}.${domain}:${P0PORT}\",
            \"tlsCACerts\": {
                \"pem\": \"${PEERPEM}\"
            },
            \"grpcOptions\": {
                \"ssl-target-name-override\": \"peer0.org${ORG}.${domain}\",
                \"hostnameOverride\": \"peer0.org${ORG}.${domain}\"
            }
        }
    },
    \"certificateAuthorities\": {
        \"ca.org${ORG}.${domain}\": {
            \"url\": \"https://ca.org${ORG}.${domain}:${CAPORT}\",
            \"caName\": \"ca-org${ORG}\",
            \"tlsCACerts\": {
                \"pem\": [\"${CAPEM}\"]
            },
            \"httpOptions\": {
                \"verify\": false
            }
        }
    }
}
"))
	(ccp-template.yaml (replace-with-values `((domain . ,cfg::base-domain))
						"---
name: test-network-org${ORG}
version: 1.0.0
client:
  organization: Org${ORG}
  connection:
    timeout:
      peer:
        endorser: '300'
organizations:
  Org${ORG}:
    mspid: Org${ORG}MSP
    peers:
    - peer0.org${ORG}.${domain}
    certificateAuthorities:
    - ca.org${ORG}.${domain}
peers:
  peer0.org${ORG}.${domain}:
    url: grpcs://peer0.org${ORG}.${domain}:${P0PORT}
    tlsCACerts:
      pem: |
          ${PEERPEM}
    grpcOptions:
      ssl-target-name-override: peer0.org${ORG}.${domain}
      hostnameOverride: peer0.org${ORG}.${domain}
certificateAuthorities:
  ca.org${ORG}.${domain}:
    url: https://ca.org${ORG}.${domain}:${CAPORT}
    caName: ca-org${ORG}
    tlsCACerts:
      pem: 
        - |
          ${CAPEM}
    httpOptions:
      verify: false
")))
    ;;Ora possiamo scrivere i files al posto giusto!!
    ;;Ma lo facciamo nel chiamante. Qui torno itre file
    (values ccp-generate.sh ccp-template.json ccp-template.yaml)))

;;
;;
(define (generate-all)
  (let ((old-wd (getcwd)))
    (try-catch
     (begin
       (chdir old-wd)
       (cerr "Uscito per errore\n"))
     ;;
     ;;mi posizione nella cartella prefix, dove tutto andrà generato
     (unless (stat cfg::prefix #f)
       (mkdir cfg::prefix))
     (chdir cfg::prefix)
     ;;
     ;;Iniziamo!
     ;;Per prima cosa, se non esiste blockchain-explorer come cartella,esce!
     (unless (and=> (stat "blockchain-explorer" #f) (lambda (st) (eqv? 'directory (stat:type st))))
       (cerr "La cartella blockchain-explorer deve essere presente all'interno di " cfg::prefix "\n")
       (error))
     ;;
     (make-directories "base")
     (make-directories "scripts")
     (with-output-to-file "core.yaml" (lambda ()  (display (core-yaml))))
     ;;(fs-io-from-string "core.yaml" (core-yaml))
     (fs-io-from-string (string-append  "base/peer-base.yaml") (peer-base-yaml cfg::network-name cfg::fabver))
     (fs-io-from-string (string-append  "base/docker-compose-base.yaml") (docker-compose-base-yaml cfg::base-domain cfg::base-port cfg::num-of-orgs))
     (fs-io-from-string (string-append  "configtx.yaml") (configtx-yaml cfg::base-domain cfg::base-port cfg::num-of-orgs))
     (fs-io-from-string (string-append  "crypto-config.yaml") (crypto-config-yaml cfg::num-of-orgs cfg::base-domain cfg::num-of-users-per-peer))
     (generate-host-up cfg::num-of-orgs cfg::network-name cfg::base-domain cfg::base-port)
     (generate-host-down cfg::num-of-orgs cfg::network-name cfg::base-domain cfg::base-port)
     (generate-host-yaml cfg::num-of-orgs cfg::network-name cfg::base-domain cfg::base-port cfg::systemchannel cfg::fabver)
     ;;
     ;;Genera ccp-generate.sh, ccp-template.json, ccp-template.yaml
     (cerr "Genera ccp-generate.sh, ccp-template.json, ccp-template.yaml")
     (receive (ccp-generate.sh ccp-template.json ccp-template.yaml) (generate-sh-json-yaml)
       (fs-io-from-string "ccp-generate.sh" ccp-generate.sh)
       (fs-io-from-string "ccp-template.json" ccp-template.json)
       (fs-io-from-string "ccp-template.yaml" ccp-template.yaml)
       )
     ;;In modo che qui dentro metto anche la chiamata a ccp-generate.sh
     (generate-make-certs-and-genesis)
     ;;
     (Show! "REMEMBER to create a docker network named: " cfg::network-name "\nExample: "
	    "docker network create " cfg::network-name)
     ;;
     ;;A seguire i comandi di creazione e join del canale
     (fs-io-from-string "create-and-join-channel.sh"
			(string-append
			 (replace-with-values `((base-domain . ,cfg::base-domain)
						(base-port . ,cfg::base-port)
						(channelid . ,cfg::channelid)
						)
					      "docker exec cli peer channel create -o orderer.${base-domain}:${base-port} -c ${channelid} -f ./channel-artifacts/${channelid}.tx --tls --cafile /opt/gopath/src/github.com/hyperledger/fabric/peer/crypto/ordererOrganizations/${base-domain}/orderers/orderer.${base-domain}/msp/tlscacerts/tlsca.${base-domain}-cert.pem")
			 "\n"
			 "sleep 5"
			 "\n"
			 (replace-with-values `((channelid . ,cfg::channelid))
					      "docker exec cli peer channel join -b ${channelid}.block")
			 "\n"
			 (string-repeater (1- cfg::num-of-orgs)
					  ((base-domain cfg::base-domain base-domain)
					   (org-num 2 (1+ org-num))
					   (org-port (+ 101 cfg::base-port) (+ 100 org-port))
					   (channelid cfg::channelid channelid)
					   )
					  "docker exec -e CORE_PEER_MSPCONFIGPATH=/opt/gopath/src/github.com/hyperledger/fabric/peer/crypto/peerOrganizations/org${org-num}.${base-domain}/users/Admin@org${org-num}.${base-domain}/msp -e CORE_PEER_ADDRESS=peer0.org${org-num}.${base-domain}:${org-port} -e CORE_PEER_LOCALMSPID=\"Org${org-num}MSP\" -e CORE_PEER_TLS_ROOTCERT_FILE=/opt/gopath/src/github.com/hyperledger/fabric/peer/crypto/peerOrganizations/org${org-num}.${base-domain}/peers/peer0.org${org-num}.${base-domain}/tls/ca.crt cli peer channel join -b ${channelid}.block\n")
			 "\n"
			 (replace-with-values `((base-domain . ,cfg::base-domain)
						(ord-port . ,cfg::base-port)
						(channelid . ,cfg::channelid)
						)
					      "docker exec cli peer channel update -o orderer.${base-domain}:${ord-port} -c ${channelid} -f ./channel-artifacts/Org1MSPanchors.tx --tls --cafile /opt/gopath/src/github.com/hyperledger/fabric/peer/crypto/ordererOrganizations/${base-domain}/orderers/orderer.${base-domain}/msp/tlscacerts/tlsca.${base-domain}-cert.pem")
			 "\n"
			 (string-repeater (1- cfg::num-of-orgs)
					  ((base-domain cfg::base-domain base-domain)
					   (org-num 2 (1+ org-num))
					   (org-port (+ 101 cfg::base-port) (+ 100 org-port))
					   (ord-port cfg::base-port ord-port)
					   (channelid cfg::channelid channelid)
					   )
					  "docker exec -e CORE_PEER_MSPCONFIGPATH=/opt/gopath/src/github.com/hyperledger/fabric/peer/crypto/peerOrganizations/org${org-num}.${base-domain}/users/Admin@org${org-num}.${base-domain}/msp -e CORE_PEER_ADDRESS=peer0.org${org-num}.${base-domain}:${org-port} -e CORE_PEER_LOCALMSPID=\"Org${org-num}MSP\" -e CORE_PEER_TLS_ROOTCERT_FILE=/opt/gopath/src/github.com/hyperledger/fabric/peer/crypto/peerOrganizations/org${org-num}.${base-domain}/peers/peer0.org${org-num}.${base-domain}/tls/ca.crt cli peer channel update -o orderer.${base-domain}:${ord-port} -c ${channelid} -f ./channel-artifacts/Org${org-num}MSPanchors.tx --tls --cafile /opt/gopath/src/github.com/hyperledger/fabric/peer/crypto/ordererOrganizations/${base-domain}/orderers/orderer.${base-domain}/msp/tlscacerts/tlsca.${base-domain}-cert.pem\n")
			 "\n"
			 "\n"
			 "\n"))
     (chmod "create-and-join-channel.sh" #o777)
     ;;
     ;;Genero i file di blockchain-explorer!!
     (cerr "Genera i file di blockchain-explorer\n")
     (generate-bc-explorer-files)
     ;;Torno nella cartella chiamante
     (chdir old-wd)))
  ;;
  ;;Stampo i codici che vanno in /etc/hosts
  (let ((hosts ""))
    (do ((org 1 (1+ org))) ((> org cfg::num-of-orgs))
      (do ((peer 0 (1+ peer))) ((>= peer cfg::num-of-peers-per-org))
	(set! hosts (string-append hosts " " "peer" (number->string peer) "." "org" (number->string org) "." cfg::base-domain))))
    (do ((ord 1 (1+ ord))) ((> ord cfg::num-of-orderers))
      (set! hosts (string-append hosts " " "orderer" (if (= 1 ord) "" (number->string ord)) "." cfg::base-domain)))
    (Show! "To use domain names instead of localhost, add this string to /etc/hosts\n" hosts)
    (Show! "Modify crontab if you want to restart the blockchain when the system restarts. use the following rule:
    crontab -e
    @reboot (sleep 30s ; cd directory_has_simplystart.sh ; ./simplystart.sh )&
    Consider that in some cases you have to add a PATH to simplystart")
    )
  ;;
  ;;ora genero il codice per la clean
  ;;
  ;;   ;;set-constants.sh
  ;;   (call-with-output-file "set-constants.sh"
  ;;     (lambda (out) (display "export DOMINIO=`jq -r '.\"base-domain\"' generatore.json`\n
  ;; export CHANNEL=`jq -r '.\"channelid\"' generatore.json`\n
  ;; export NUMORGS=`jq -r '.\"num-of-orgs\"' generatore.json`\n
  ;; export CHAINCODE=`jq -r '.\"chaincode\"' generatore.json`\n
  ;; export CHAINCODE_FOLDER=`jq -r '.\"chaincode-folder\"' generatore.json`\n
  ;; export VERSION=`jq -r '.\"chaincode-version\"' generatore.json`\n
  ;; export NETWORK_NAME=`jq -r '.\"network-name\"' generatore.json`\n
  ;; echo $DOMINIO, $CHANNEL, $NUMORGS, $CHAINCODE, $CHAINCODE_FOLDER, $VERSION, $NETWORK_NAME\n
  ;; " out)))
  ;;RunMeFirst
  ;; (call-with-output-file "RunMeFirst"
;;     (lambda (out)
;;       (display
;; "#Provo a verificare se cuda è presente\n
;; nvidia-smi >/dev/null 2>/dev/null\n
;; cudaok=$?\n
;; if [[ \"$cudaok\" == \"0\" ]]\n
;; then\n
;;     echo \"Run with cuda\"\n
;;     docker run -ti --privileged --net host --gpus=\"all\" --volume \"$PWD\":/vapps -e \"DISPLAY=$DISPLAY\" -v /tmp/.X11-unix:/tmp/.X11-unix:rw --device /dev/dri -v /:/altro --volume=\"$HOME/.Xauthority:/root/.Xauthority:rw\" --volume /usr/local/zed/resources/:/usr/local/zed/resources/ dozenapps/va:latest /bin/bash -c 'cd /vapps; LD_LIBRARY_PATH=\"/new_devs/usr/local/shared/lib64/:/usr/local/cuda-11.7/compat/:$LD_LIBRARY_PATH\" guile -C . -l run.scm -c \"(generate-all)\"'\n
;; else\n
;;     echo \"Run without cuda\"\n
;;     docker run -ti --net host --volume \"$PWD\":/vapps -e \"DISPLAY=$DISPLAY\" -v /tmp/.X11-unix:/tmp/.X11-unix:rw --device /dev/dri -v /:/altro --volume=\"$HOME/.Xauthority:/root/.Xauthority:rw\" --volume /usr/local/zed/resources/:/usr/local/zed/resources/ dozenapps/va:latest /bin/bash -c 'cd /vapps; LD_LIBRARY_PATH=\"/new_devs/usr/local/shared/lib64/:/usr/local/cuda-11.7/compat/:$LD_LIBRARY_PATH\" guile -C . -l run.scm -c \"(generate-all)\"'\n
;; fi\n"  out)))
  ;; (system "chmod a+rwx RunMeFirst")
  ;;
  ;;clean-all.sh
  (call-with-output-file "clean-all.sh"
    (lambda (out) (display 
		   (string-append
		    "echo 'Beware, this script cleans all the blockchain data, scritps and local folder'
read -n 1 -s -r -p \"Press any key to continue, CTRL-C to stop\"\n"
		    (string-repeater cfg::num-of-orgs
				     ((i 1 (1+ i)))
				     "./host${i}down.sh -v --remove-orphans\n
sudo /bin/rm -f ./host${i}down.sh\n
sudo /bin/rm -f ./host${i}up.sh\n
sudo /bin/rm -f ./host${i}.yaml\n")
		    "sudo /bin/rm -rf keystore wallet ##Se ricominci allora cancella anche il wallet\n
sudo /bin/rm -rf base channel-artifacts crypto-config scripts   ##Per pulire per benino\n
sudo /bin/rm -f ccp-generate.sh ccp-template.json ccp-template.yaml configtx.yaml create-and-join-channel.sh crypto-config.yaml make-cert-and-genesis.sh output.log.1\n
") out)))
  (system "chmod a+rwx clean-all.sh")
  ;;
  ;;clean-blockchain.sh
  (call-with-output-file "clean-blockchain.sh"
    (lambda (out)
      (display (string-append
		"echo 'Beware, this script cleans all the blockchain data'
read -n 1 -s -r -p \"Press any key to continue, CTRL-C to stop\"\n"
		(string-repeater cfg::num-of-orgs
				 ((i 1 (1+ i)))
				 "./host${i}down.sh -v --remove-orphans\n")) out)))
  (system "chmod a+rwx clean-blockchain.sh")
  ;;
  ;;stop-blockchain.sh
  (call-with-output-file "stop-blockchain-clean-chaincode.sh"
    (lambda (out)
      (display (string-repeater cfg::num-of-orgs
			((i 1 (1+ i)))
			"./host${i}down.sh -v --remove-orphans\n") out)))
  (system "chmod a+rwx stop-blockchain-clean-chaincode.sh")
  ;;
  (call-with-output-file "stop-blockchain.sh"
    (lambda (out)
      (display (string-repeater cfg::num-of-orgs
			((i 1 (1+ i)))
			"./host${i}down.sh\n") out)))
  (system "chmod a+rwx stop-blockchain.sh")
  ;;
  ;;Clean all and start
  (call-with-output-file "clean-start.sh"
    (lambda (out)
      (display (string-append
		"echo 'Beware, this script cleans all data and rewrites all'
echo 'using the configuration files starting from cgeneratore.json contents'
read -n 1 -s -r -p \"Press any key to continue, CTRL-C to stop\"\n
\n"
		(string-repeater cfg::num-of-orgs
				 ((i 1 (1+ i)))
				 "./host${i}down.sh -v --remove-orphans\n")
		(string-repeater cfg::num-of-orgs
				 ((i 1 (1+ i)))
				 "./host${i}down.sh -v --remove-orphans\n
sudo /bin/rm -f ./host[0-9]*down.sh\n
sudo /bin/rm -f ./host[0-9]*up.sh\n
sudo /bin/rm -f ./host[0-9]*.yaml\n")
		"sudo /bin/rm -rf keystore wallet ##Se ricominci allora cancella anche il wallet\n
sudo /bin/rm -rf base channel-artifacts crypto-config scripts   ##Per pulire per benino\n
sudo /bin/rm -f ccp-generate.sh ccp-template.json ccp-template.yaml configtx.yaml create-and-join-channel.sh crypto-config.yaml make-cert-and-genesis.sh output.log.1\n"
		"./RunMeFirst\n
read -n 1 -s -r -p \"archive these ip numbers and press any key to continue, CTRL-C to stop\"\n
\n
sudo chown -R ${USER}:${USER} *\n"
				
		(replace-with-values `(("NETWORK_NAME" . ,cfg::network-name)
				      ("DOMINIO" . ,cfg::base-domain)
				      ("CHANNEL" . ,cfg::channelid))
				     "docker network create ${NETWORK_NAME}\n
./make-cert-and-genesis.sh\n"
				     )
		(string-repeater cfg::num-of-orgs ((i 1 (1+ i))) "./host${i}up.sh\n")
		"sleep 3\n"
		"./create-and-join-channel.sh\n"
		"sleep 2\n"
		(string-repeater cfg::num-of-orgs ((i 1 (1+ i))
						   (DOMINIO cfg::base-domain cfg::base-domain)
						   (CHANNEL cfg::channelid cfg::channelid))
				 " docker exec peer0.org${i}.${DOMINIO} peer channel getinfo -c ${CHANNEL}\n"))
	       out)))
  (system "chmod a+rwx clean-start.sh")
;;
(call-with-output-file "simply-start.sh"
  (lambda (out)
    (display (string-append
	      (string-repeater cfg::num-of-orgs ((i 1 (1+ i))) "./host${i}up.sh\n")
	      "./create-and-join-channel.sh\n"
	      (string-repeater cfg::num-of-orgs ((i 1 (1+ i))
						 (DOMINIO cfg::base-domain cfg::base-domain)
						 (CHANNEL cfg::channelid cfg::channelid))
			       "docker exec peer0.org${i}.${DOMINIO} peer channel getinfo -c ${CHANNEL}\n"))
	     out)))
  (system "chmod a+rwx simply-start.sh")
;;

(call-with-output-file "compile-and-deploy-chaincode.sh"
  (lambda (out)
    (display
     (string-append
      (replace-with-values `(
			    ("CHAINCODE_FOLDER" . ,cfg::chaincode-folder)
			    ("CHAINCODE" . ,cfg::chaincode))
			   "#pushd ${CHAINCODE_FOLDER}\n
#GO111MODULE=on go mod vendor\n
#popd\n
docker exec cli peer lifecycle chaincode package ${CHAINCODE}.tar.gz --path /opt/gopath/src/github.com/chaincode/ --label ${CHAINCODE}_1\n")
			   (string-repeater cfg::num-of-orgs
					    ((i 1 (1+ i))
					     (CHAINCODE cfg::chaincode cfg::chaincode)
					     (DOMINIO cfg::base-domain cfg::base-domain)
					     (PORT (1+ cfg::base-port) (+ 100 PORT)))
					    "docker exec -e CORE_PEER_MSPCONFIGPATH=/opt/gopath/src/github.com/hyperledger/fabric/peer/crypto/peerOrganizations/org${i}.${DOMINIO}/users/Admin@org${i}.${DOMINIO}/msp -e CORE_PEER_ADDRESS=peer0.org${i}.${DOMINIO}:${PORT} -e CORE_PEER_LOCALMSPID=\"Org${i}MSP\" -e CORE_PEER_TLS_ROOTCERT_FILE=/opt/gopath/src/github.com/hyperledger/fabric/peer/crypto/peerOrganizations/org${i}.${DOMINIO}/peers/peer0.org${i}.${DOMINIO}/tls/ca.crt cli peer lifecycle chaincode install ${CHAINCODE}.tar.gz\n")
			   (replace-with-values `(
						 ("CHAINCODE_FOLDER" . ,cfg::chaincode-folder)
						 ("CHAINCODE" . ,cfg::chaincode)
						 ("CHANNEL" . ,cfg::channelid)
						 ("DOMINIO" . ,cfg::base-domain)
						 ("PORT" . ,(1+ cfg::base-port)))
						"export CHAINCODEID=`docker exec -e CORE_PEER_MSPCONFIGPATH=/opt/gopath/src/github.com/hyperledger/fabric/peer/crypto/peerOrganizations/org1.${DOMINIO}/users/Admin@org1.${DOMINIO}/msp -e CORE_PEER_ADDRESS=peer0.org1.${DOMINIO}:${PORT} -e CORE_PEER_LOCALMSPID=\"Org1MSP\" -e CORE_PEER_TLS_ROOTCERT_FILE=/opt/gopath/src/github.com/hyperledger/fabric/peer/crypto/peerOrganizations/org1.${DOMINIO}/peers/peer0.org1.${DOMINIO}/tls/ca.crt cli peer lifecycle chaincode queryinstalled -O json|jq -r \".installed_chaincodes[0].package_id\"`\n
echo Recuperato il chaincode id: $CHAINCODEID\n
\n
#Esiste jq?\n
jq --version >/dev/null 2>/dev/null\n
jqok=$?\n
if [[ \"$jqok\" != \"0\" ]]\n
then\n
  sudo apt update & sudo apt install -y jq\n
fi\n
\n
function GiveLatestChaincodeSequence () {\n
    sequence=`docker exec cli peer lifecycle chaincode queryapproved --channelID ${CHANNEL} --name ${CHAINCODE} -O json 2>/dev/null`\n
    if [ $? -eq 0 ];\n
    then\n
        echo `echo $sequence | jq -r \".sequence\"`\n
    else\n
        echo 0\n
    fi\n
}\n
export SEQUENCE=$(GiveLatestChaincodeSequence)\n
SEQUENCE=$((SEQUENCE + 1))\n
echo \"Sequence calcolato: \" $SEQUENCE\n
\n")
			   (string-repeater cfg::num-of-orgs
					    ((i 1 (1+ i))
					     (CHAINCODE cfg::chaincode cfg::chaincode)
					     (DOMINIO cfg::base-domain cfg::base-domain)
					     (PORT (1+ cfg::base-port) (+ 100 PORT))
					     (CHANNEL cfg::channelid  cfg::channelid)
					     (VERSION cfg::chaincode-version cfg::chaincode-version)
					     )
					    "docker exec -e CORE_PEER_MSPCONFIGPATH=/opt/gopath/src/github.com/hyperledger/fabric/peer/crypto/peerOrganizations/org${i}.${DOMINIO}/users/Admin@org${i}.${DOMINIO}/msp -e CORE_PEER_ADDRESS=peer0.org${i}.${DOMINIO}:${PORT} -e CORE_PEER_LOCALMSPID=\"Org${i}MSP\" -e CORE_PEER_TLS_ROOTCERT_FILE=/opt/gopath/src/github.com/hyperledger/fabric/peer/crypto/peerOrganizations/org${i}.${DOMINIO}/peers/peer0.org${i}.${DOMINIO}/tls/ca.crt cli peer lifecycle chaincode approveformyorg --tls --cafile /opt/gopath/src/github.com/hyperledger/fabric/peer/crypto/ordererOrganizations/${DOMINIO}/orderers/orderer.${DOMINIO}/msp/tlscacerts/tlsca.${DOMINIO}-cert.pem --channelID ${CHANNEL} --name ${CHAINCODE} --version ${VERSION} --sequence ${SEQUENCE} --waitForEvent --package-id ${CHAINCODEID}\n")
			    
			   (replace-with-values `(("CHAINCODE" . ,cfg::chaincode)
						 ("CHANNEL" . ,cfg::channelid)
						 ("VERSION" . ,cfg::chaincode-version)
						 ("DOMINIO" . ,cfg::base-domain)
						 ("PORT" . ,cfg::base-port))
						"docker exec cli peer lifecycle chaincode checkcommitreadiness --channelID ${CHANNEL} --name ${CHAINCODE} --version ${VERSION} --sequence ${SEQUENCE}\n
docker exec cli peer lifecycle chaincode commit -o orderer.${DOMINIO}:${PORT} --tls --cafile /opt/gopath/src/github.com/hyperledger/fabric/peer/crypto/ordererOrganizations/${DOMINIO}/orderers/orderer.${DOMINIO}/msp/tlscacerts/tlsca.${DOMINIO}-cert.pem \\")
			   (string-repeater cfg::num-of-orgs
					    ((DOMINIO cfg::base-domain cfg::base-domain)
					     (PORT (1+ cfg::base-port) (+ 100 PORT))
					    (org 1 (1+ org)))
					    "--peerAddresses peer0.org${org}.${DOMINIO}:${PORT} --tlsRootCertFiles /opt/gopath/src/github.com/hyperledger/fabric/peer/crypto/peerOrganizations/org${org}.${DOMINIO}/peers/peer0.org${org}.${DOMINIO}/tls/ca.crt \\")
			   (replace-with-values `(("CHAINCODE" . ,cfg::chaincode)
						 ("CHANNEL" . ,cfg::channelid)
						 ("VERSION" . ,cfg::chaincode-version)
						 ("DOMINIO" . ,cfg::base-domain)
						 ("PORT" . ,cfg::base-port))
						"--channelID ${CHANNEL} --name ${CHAINCODE} --version ${VERSION} --sequence ${SEQUENCE}\n\n
docker exec cli peer lifecycle chaincode querycommitted --channelID ${CHANNEL} --name ${CHAINCODE}\n"))
			   out)))
  (system "chmod a+rwx compile-and-deploy-chaincode.sh")
)
