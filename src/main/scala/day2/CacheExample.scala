package day2

object CacheExample {

  case class Cache[K, V](data: Map[K, V], hits: Int, misses: Int)
  type CustomerCache = Cache[String, CustomerInfo]


  trait CustomerInfo

  case class SimpleCustomerInfo(name: String) extends CustomerInfo

  trait CustomerService {
    def retrieveCustomerInfo(name: String, cache: Cache[String, CustomerInfo]): (CustomerInfo, Cache[String, CustomerInfo])
  }

  class CustomerServiceImpl extends CustomerService {

    def checkInCache(name: String, cache: Cache[String, CustomerInfo]): Option[CustomerInfo] = {
      val res = cache.data.get(name)
      res
    }


    def retrieveCustomerInfo(name: String, cache: Cache[String, CustomerInfo]): (CustomerInfo, Cache[String, CustomerInfo]) = {
      checkInCache(name, cache) match {
        case Some(value) => (value, cache.copy(hits = cache.hits + 1))
        case None =>
          Thread.sleep(200)
          val res = SimpleCustomerInfo(name)
          (res, cache.copy(data = cache.data + (name -> res), misses = cache.misses + 1))
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val service = new CustomerServiceImpl()
    val start = System.currentTimeMillis()
    val cache = Cache(Map.empty[String,CustomerInfo], 0, 0)
    val (r1, cache1: CustomerCache) = service.retrieveCustomerInfo("John", cache)
    val (r2, cache2: CustomerCache) = service.retrieveCustomerInfo("Jane", cache1)
    val (r3, cache3: CustomerCache) = service.retrieveCustomerInfo("John", cache2)
    val (r4, cache4: CustomerCache) = service.retrieveCustomerInfo("Jane", cache3)
    val end = System.currentTimeMillis()
    println("results: " + List(r1, r2, r3, r4))
    println(s"took: ${end - start} millis")
    println(s"cache: ${cache4}")
  }


}
