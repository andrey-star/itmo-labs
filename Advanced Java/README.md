# Advanced Java course

## 1. [RecursiveWalk](https://github.com/andrey-star/itmo-labs/tree/master/Advanced%20Java/RecursiveWalk/ru.ifmo.rain.starodubtsev.walk) (IO, NIO)

Create a ```RecursiveWalk``` [class](https://github.com/andrey-star/itmo-labs/blob/master/Advanced%20Java/RecursiveWalk/ru.ifmo.rain.starodubtsev.walk/ru/ifmo/rain/starodubtsev/walk/RecursiveWalk.java), capable of computing file hash sums.
* CMD arguments: ```<input file> <output file>```
* Input file contains a list of files and directories to walk recursively.
* The output file should contain one line per file in the following format:
    ```<hexadecimal hash sum> <path to file>```
* For hash sum computation use the [FNV](https://ru.wikipedia.org/wiki/FNV) algorithm.
* If errors occur when working with a file, the hash sum should be set to ```00000000```
* Input and output file encoding: ```UTF-8```
* If the output file parent directory doesn't exist, it should be created.
* File sizes could exceed available memory.

## 2. [ArraySet](https://github.com/andrey-star/itmo-labs/tree/master/Advanced%20Java/ArraySet/ru.ifmo.rain.starodubtsev.arrayset) (Collections)
Create an ```ArraySet``` [class](https://github.com/andrey-star/itmo-labs/blob/master/Advanced%20Java/ArraySet/ru.ifmo.rain.starodubtsev.arrayset/ru/ifmo/rain/starodubtsev/arrayset/ArraySet.java), implementing an immutable sorted set data structure.
* The class should implement the ```NavigableSet``` interface.
* All operations should have the optimal time complexity.

## 3. [StudentDB](https://github.com/andrey-star/itmo-labs/tree/master/Advanced%20Java/StudentDB/ru.ifmo.rain.starodubtsev.student) (Lambdas, Streams)
Create a ```StudentDB``` [class](https://github.com/andrey-star/itmo-labs/blob/master/Advanced%20Java/StudentDB/ru.ifmo.rain.starodubtsev.student/ru/ifmo/rain/starodubtsev/student/StudentDB.java), capable of searching a database of students.
* Each method should consist of exactly one operator. Long operators should be chopped down.

## 4. [Implementor](https://github.com/andrey-star/itmo-labs/tree/master/Advanced%20Java/Implementor/ru.ifmo.rain.starodubtsev.implementor) (Reflection)
Create an ```Implementor``` [class](https://github.com/andrey-star/itmo-labs/blob/master/Advanced%20Java/Implementor/ru.ifmo.rain.starodubtsev.implementor/ru/ifmo/rain/starodubtsev/implementor/Implementor.java), capable of generating class and interface implementations.
* CMD arguments: ```<full class name>```
* The program should generate the java-code of a class with ```Impl``` suffix, extending (implementing) the provided class (interface).
* The generated class should compile without errors.
* The generated class should not be abstract.
* The methods of the generated class should ignore their arguments and return default values.

## 5. [JarImplementor](https://github.com/andrey-star/itmo-labs/tree/master/Advanced%20Java/Implementor/ru.ifmo.rain.starodubtsev.implementor) (Jar, Modules)
* Create a ```.jar``` file, containing the compiled ```Implementor``` and associated classes.
    * The created ```.jar``` file should be executable with ```java -jar``` command.
    * The executable ```.jar``` file should accept the same arguments as ```Implementor```.
* Modify ```Implementor``` so that, when executed with ```-jar <class name> <jar file>``` arguments, it generates
a ```.jar``` file with the implementation of the provided class (interface).
* Create a [script](https://github.com/andrey-star/itmo-labs/blob/master/Advanced%20Java/run/implementor/build_jar.cmd) that produces the executable ```.jar``` file.
* The solution should be modularized.

## 6. [Javadoc](https://github.com/andrey-star/itmo-labs/tree/master/Advanced%20Java/Implementor/ru.ifmo.rain.starodubtsev.implementor)
* Document the ```Implementor``` class and associated classes using *Javadoc*.
    * All classes and their members should be documented, including ```private``` ones.
    * The documentation should generate without warnings.
    * The generated documentation should contain valid links to classes from the standard library.
* Create a [script](https://github.com/andrey-star/itmo-labs/blob/master/Advanced%20Java/run/implementor/java_doc.cmd) generating the documentation.

## 7. [IterativeParallelism](https://github.com/andrey-star/itmo-labs/tree/master/Advanced%20Java/IterativeParallelism/ru.ifmo.rain.starodubtsev.concurrent) (Concurrency, Threads)
Create an ```IterativeParallelism``` [class](https://github.com/andrey-star/itmo-labs/blob/master/Advanced%20Java/IterativeParallelism/ru.ifmo.rain.starodubtsev.concurrent/ru/ifmo/rain/starodubtsev/concurrent/IterativeParallelism.java), capable of processing lists in multiple threads.
* The following methods should be implemented:
    * ```minimum(threads, list, comparator)``` - first minimum element.
    * ```maximum(threads, list, comparator)``` - first maximum element.
    * ```all(threads, list, predicate)``` - checks, if all elements match the predicate.
    * ```any(threads, list, predicate)``` - checks, if any elements match the predicate.
    * ```filter(threads, list, predicate)``` - returns a list of elements, matching the predicate.
    * ```map(threads, list, function)``` - returns a list of elements, mapped by the provided function.
    * ```join(threads, list)``` - concatenation of list's elements string representations.
* All methods receive a ```threads``` parameter - how many threads should be used for computation.
* The provided comparators and functions are not guaranteed to be fast.
* You cannot use *Concurrency Utilities* in this task.

## 8. [ParallelMapper](https://github.com/andrey-star/itmo-labs/blob/master/Advanced%20Java/IterativeParallelism/ru.ifmo.rain.starodubtsev.concurrent) (Concurrency, Synchronization, wait/notify)
Create a ```ParallelMapperImpl``` [class](https://github.com/andrey-star/itmo-labs/blob/master/Advanced%20Java/IterativeParallelism/ru.ifmo.rain.starodubtsev.concurrent/ru/ifmo/rain/starodubtsev/concurrent/ParallelMapperImpl.java), implementing the ```ParallelMapper``` interface.
 ```
public interface ParallelMapper extends AutoCloseable {
    <T, R> List<R> run(
        Function<? super T, ? extends R> f,
        List<? extends T> args
    ) throws InterruptedException;

    @Override
    void close() throws InterruptedException;
}
```
* The ```run``` method should compute the function on each of the provided methods.
* The ```close``` method should stop all working threads.
* ```ParallelMapperImpl(threads)``` constructor should create a provided number of working threads, which can be used
for parallel computation.
* Multiple clients may use one ```ParallelMapper``` simultaneously.
* New tasks should be stored in a queue and processed in order.
* The implementation should not have busy wait.
* Modify the ```ItertiveParallelism``` class to make use of ```ParallelMapperImpl```:
    * Add an ```IterativeParallelism(ParallelMapper)``` constructor.
    * Class methods should split the task into ```threads``` sub tasks and process them using ```ParallelMapper```
    * The ability to run multiple clients simultaneously making use of one ```ParallelMapper``` should be supported.
    * If a ```ParallelMapper``` is provided, ```ItertiveParallelism``` should not create new threads.
    
## 9. [WebCrawler](https://github.com/andrey-star/itmo-labs/tree/master/Advanced%20Java/WebCrawler/ru.ifmo.rain.starodubtsev.crawler) (Concurrency Utilities)
Create a thread safe ```WebCrawler``` [class](https://github.com/andrey-star/itmo-labs/blob/master/Advanced%20Java/WebCrawler/ru.ifmo.rain.starodubtsev.crawler/ru/ifmo/rain/starodubtsev/crawler/WebCrawler.java), capable of traversing websites recursively.
* CMD arguments: ```url [depth [downloads [extractors [perHost]]]]```
* The class should have a ```public WebCrawler(Downloader downloader, int downloaders, int extractors, int perHost)``` constructor.
    * downloader - class capable of downloading pages and extracting links from them.
    * downloaders - maximum number of concurrently downloading pages.
    * extractors - maximum number of concurrent link extractions.
    * perHost - maximum number of pages simultaneously downloading from the same host.
* The ```WebCrawler``` class should implement the ```Crawler``` interface:
```
public interface Crawler extends AutoCloseable {
    Result download(String url, int depth);
    void close();
}
``` 
* The ```download``` method should traverse pages recursively starting form the provided ```url``` 
and to the provided ```depth```, returning a list of downloaded pages and files. This method may be called concurrently in multiple threads.
* The ```close``` method should stop all working threads. 

## 10. [HelloUDP](https://github.com/andrey-star/itmo-labs/tree/master/Advanced%20Java/HelloUDP/ru.ifmo.rain.starodubtsev.hello) (Net, Sockets)
Create a client and a server communicating via UDP.
* ```HelloUDPClient``` [class](https://github.com/andrey-star/itmo-labs/blob/master/Advanced%20Java/HelloUDP/ru.ifmo.rain.starodubtsev.hello/ru/ifmo/rain/starodubtsev/hello/HelloUDPClient.java) should send requests to the server, accept responses and output them.
    * CMD arguments: ```<host> <port> <prefix> <threads> <requests>```
        * host - name or ip address of the server.
        * port - the port listening for requests.
        * prefix - request prefix.
        * threads - number of concurrent threads.
        * requests - number of requests in each thread.
    * Requests should be sent concurrently in the provided number of threads. 
    Each thread waits for the request to be processed and outputs the response. If the request wasn't answered, it should be sent again.
    * Requests are created in following form: ```<prefix><threadId>_<requestId>```
* ```HelloUDPServer``` [class](https://github.com/andrey-star/itmo-labs/blob/master/Advanced%20Java/HelloUDP/ru.ifmo.rain.starodubtsev.hello/ru/ifmo/rain/starodubtsev/hello/HelloUDPServer.java) should receive requests from the client and respond to them.
    * CMD arguments: ```<port> <threads>```
        * port - the port on which to listen to requests.
        * threads - the number of threads to process the requests.
    * The response should be ```Hello, <request body>```
    * If the server can't handle the amount of requests, excess requests can be ignored.
    
## 11. [Bank](https://github.com/andrey-star/itmo-labs/tree/master/Advanced%20Java/Bank/ru.ifmo.rain.starodubtsev.bank) (RMI, JUnit)
Create a banking [application](https://github.com/andrey-star/itmo-labs/blob/master/Advanced%20Java/Bank/ru.ifmo.rain.starodubtsev.bank/ru/ifmo/rain/starodubtsev/bank/main/bank/RemoteBank.java) capable of working with persons.
* A ```Person``` should have a first name, a last name and a passport id.
* ```LocalPerson``` should be sent using serialization.
* ```RemotePerson``` should be sent using remote objects.
* The bank should be capable of finding persons by passport id, supporting both types.
* A new ```Person``` can be created using their credentials.
* A ```Person``` can have multiple accounts and should be able to access them.
* A person's account with the id ```subId``` should correspond to a bank account with id ```passport:subId```
* Changes applied to a bank account should be visible to all corresponding ```RemotePerson``` and only to those
```LocalPerson```, which where created after the change.
* Changes to ```RemotePerson``` should be applied globally. 
Changes to ```LocalPerson``` should only be applied locally to this ```LocalPerson```
* Create an [application](https://github.com/andrey-star/itmo-labs/blob/master/Advanced%20Java/Bank/ru.ifmo.rain.starodubtsev.bank/ru/ifmo/rain/starodubtsev/bank/main/Client.java) demonstrating the bank features:
    * CMD arguments: ```<first name> <last name> <passport id> <account id> <amount>```
    * If the person doesn't exist it should be added. Otherwise, the credentials should be verified.
    * If the person doesn't have an account with the provided id, it should be created with zero balance.
    * After updating output the new balance.
* Write [tests](https://github.com/andrey-star/itmo-labs/tree/master/Advanced%20Java/Bank/ru.ifmo.rain.starodubtsev.bank/ru/ifmo/rain/starodubtsev/bank/test), checking the behaviour of both the bank and the client.
    * For testing the [JUnit](https://junit.org/junit5/) framework is recommended. 
    * The tests shouldn't count on a started RMI Registry.
    * Create a ```BankTests``` [class](https://github.com/andrey-star/itmo-labs/blob/master/Advanced%20Java/Bank/ru.ifmo.rain.starodubtsev.bank/ru/ifmo/rain/starodubtsev/bank/test/BankTests.java) which runs the tests.
    * Create a [script](https://github.com/andrey-star/itmo-labs/blob/master/Advanced%20Java/run/bank/bank_tests.cmd) that runs ```BankTests``` and returns a status code of 0, if tests succeed and 1, if they fail.
    * Create a [script](https://github.com/andrey-star/itmo-labs/blob/master/Advanced%20Java/run/bank/junit_bank_tests.cmd) that runs the tests using the default approach for your framework.
    The status code should be the same as described previously.

## 12. [HelloNonblockingUDP](https://github.com/andrey-star/itmo-labs/tree/master/Advanced%20Java/HelloUDP/ru.ifmo.rain.starodubtsev.hello) (Non-blocking & Asynchronous IO)
Create a client and a server communicating via UDP utilizing non-blocking IO.
* ```HelloUDPNonblockingClient``` [class](https://github.com/andrey-star/itmo-labs/blob/master/Advanced%20Java/HelloUDP/ru.ifmo.rain.starodubtsev.hello/ru/ifmo/rain/starodubtsev/hello/HelloUDPNonblockingClient.java) should have the same functionality as ```HelloUDPClient```, but without creating new threads.
* ```HelloUDPNonblockingServer``` [class](https://github.com/andrey-star/itmo-labs/blob/master/Advanced%20Java/HelloUDP/ru.ifmo.rain.starodubtsev.hello/ru/ifmo/rain/starodubtsev/hello/HelloUDPNonblockingServer.java) should have the same functionality as ```HelloUDPServer```, but all operations on the socket should be done in one thread.
* The implementation should not have busy wait.
* Duplicate code from the old and new implementations should be extracted.

