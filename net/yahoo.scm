(define-module net.yahoo
  (use gauche.parameter)
  (use rfc.http)
  (use rfc.uri)
  (use srfi-1)
  (use srfi-13)
  (use sxml.ssax)
  (use sxml.sxpath)
  (use sxml.tools)
  (use text.tree)
  (use util.list)
  (use util.match)
  (define map/index
    (with-module gauche.sequence map-with-index))
  (export yj-application-id <yj-api-error> yj-api-error?))

(select-module net.yahoo)

(define yj-application-id
  (make-parameter "YahooDemo"))

(define *namespaces*
  '("urn:yahoo:api"
    "urn:yahoo:jp:srch"
    "urn:yahoo:jp:srchmi"
    "urn:yahoo:jp:srchmv"
    "urn:yahoo:jp:srchunit"
    "urn:yahoo:jp:dir:tree"
    "urn:yahoo:jp:dir:srch"
    "urn:yahoo:jp:auc:category"
    "urn:yahoo:jp:auc:leaf"
    "urn:yahoo:jp:auc:sellinglist"
    "urn:yahoo:jp:auc:search"
    "urn:yahoo:jp:auc:item"
    "urn:yahoo:jp:auc:BidHistory"
    "urn:yahoo:jp:auc:BidHistoryDetail"
    "urn:yahoo:jp:auc:ShowQandA"
    "urn:yahoo:jp:auc:ShowRating"
    "urn:yahoo:jp:music:station"
    "urn:yahoo:jp:maps:tree"
    "urn:yahoo:jp:jlp"
    "urn:yahoo:jp:jlp:JIMService"
    "urn:yahoo:jp:jlp:FuriganaService"
    "urn:yahoo:jp:jlp:KouseiService"
    "urn:yahoo:jp:jlp:DAService"
    "urn:yahoo:jp:jlp:DAServiceSearch"
    "urn:yahoo:jp:news"
    "urn:yahoo:jp:cert"
    "urn:yahoo:jp:chiebukuro"
    "urn:yahoo:jp:itemSearch"
    "urn:yahoo:jp:categoryRanking"
    "urn:yahoo:jp:categorySearch"
    "urn:yahoo:jp:itemLookup"
    "urn:yahoo:jp:queryRanking"
    "urn:yahoo:jp:Content"
    "urn:yahoo:jp:getModule"))

(define-condition-type <yj-api-error> <error>
  yj-api-error?
  (server #f)
  (request-uri #f)
  (status #f)
  (headers #f)
  (body #f))

(define (xml->sxml xml)
  (call-with-input-string xml
    (cut ssax:xml->sxml <> (map (cut cons #f <>) *namespaces*))))

(define /Error/Message
  (sxpath '(Error Message)))

(define (make-message-pattern status)
  (string->regexp (string-append "<title>Yahoo! - " status " ([^<]+)")))

(define (extract-error-message status body)
  (guard (e (else (rxmatch-if ((make-message-pattern status) body)
                      (#f message)
                    message
                    #f)))
    (string-trim-both (cadar (/Error/Message (xml->sxml body))))))

(define (make-error-message server request-uri status headers body)
  (let ((message (extract-error-message status body)))
    (tree->string (if (not message)
                      `(,status)
                      `(,status " " ,message)))))

(define (yj-api-error server request-uri status headers body)
  (error <yj-api-error>
         :server server :request-uri request-uri
         :status status :headers headers :body body
         (make-error-message server request-uri status headers body)))

(define (make-query arguments)
  (tree->string
   (map/index (lambda (i x)
                (let ((x* (uri-encode-string (x->string x))))
                  (cond ((zero? i) x*)
                        ((even? i) (cons '& x*))
                        (else (cons '= x*)))))
              arguments)))

(define (make-query/appid arguments)
  (make-query `(:appid ,(yj-application-id) ,@arguments)))

(define-syntax request
  (syntax-rules (GET POST)
    ((_ GET server path query option ...)
     (http-get server (string-append path "?" query) option ...))
    ((_ POST server path query option ...)
     (http-post server path query
                :Content-Type "application/x-www-form-urlencoded"
                option ...))))

(define-syntax define-api
  (syntax-rules ()
    ((_ name method server path)
     (define (name . arguments)
       (let*-values (((query)               (make-query/appid arguments))
                     ((status headers body) (request method server path query)))
         (unless (string=? status "200")
           (yj-api-error server query status headers body))
         (let ((sxml (xml->sxml body)))
           (sxml:squeeze! sxml)
           sxml))))))

(define-syntax define&export-apis
  (syntax-rules ()
    ((_ (server method path ...) ...)
     (begin
       (begin (define-api path method server (symbol->string 'path)) ...) ...
       (begin (export path) ...) ...))))

(define&export-apis
 ("search.yahooapis.jp" POST
  /WebSearchService/V1/webSearch
  /ImageSearchService/V1/imageSearch
  /VideoSearchService/V1/videoSearch
  /AssistSearchService/V1/webunitSearch)
 ("map.yahooapis.jp" GET
  /LocalSearchService/V1/LocalSearch)
 ("jlp.yahooapis.jp" POST
  /MAService/V1/parse
  /JIMService/V1/conversion
  /FuriganaService/V1/furigana
  /KouseiService/V1/kousei
  /DAService/V1/parse
  /DAService/V1/search)
 ("auctions.yahooapis.jp" POST
  /AuctionWebService/V1/CategoryTree
  /AuctionWebService/V1/CategoryLeaf
  /AuctionWebService/V1/SellingList
  /AuctionWebService/V1/Search
  /AuctionWebService/V1/AuctionItem
  /AuctionWebService/V1/BidHistory
  /AuctionWebService/V1/BidHistoryDetail
  /AuctionWebService/V1/ShowQandA
  /AuctionWebService/V1/ShowRating)
 ("shopping.yahooapis.jp" GET
  /ShoppingWebService/V1/itemSearch
  /ShoppingWebService/V1/categoryRanking
  /ShoppingWebService/V1/categorySearch
  /ShoppingWebService/V1/itemLookup
  /ShoppingWebService/V1/queryRanking
  /ShoppingWebService/V1/contentMatchItem
  /ShoppingWebService/V1/contentMatchRanking
  /ShoppingWebService/V1/getModule)
 ("news.yahooapis.jp" GET
  /NewsWebService/V1/Topics)
 ("chiebukuro.yahooapis.jp" GET
  /Chiebukuro/V1/questionSearch
  /Chiebukuro/V1/categoryTree)
 ("dir.yahooapis.jp" GET
  /Category/V1/Category
  /Category/V1/directorySearch)
 ("cert.yahooapis.jp" GET
  /MinnaCertWebService/V1/certList
  /MinnaCertWebService/V1/certDetail
  /MinnaCertWebService/V1/certExam)
 ("station.music.yahooapis.jp" GET
  /StationWebService/V1/ProgramList))

(provide "net.yahoo")
