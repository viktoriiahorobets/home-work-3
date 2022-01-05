package futures

import scala.annotation.tailrec
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

object task_futures_sequence {

  /**
   * В данном задании Вам предлагается реализовать функцию fullSequence,
   * похожую на Future.sequence, но в отличии от нее,
   * возвращающую все успешные и не успешные результаты.
   * Возвращаемое тип функции - кортеж из двух списков,
   * в левом хранятся результаты успешных выполнений,
   * в правово результаты неуспешных выполнений.
   * Не допускается использование методов объекта Await и мутабельных переменных var
   */
  /**
   * @param futures список асинхронных задач
   * @return асинхронную задачу с кортежом из двух списков
   */
  def fullSequence[A](futures: List[Future[A]])
                     (implicit ex: ExecutionContext): Future[(List[A], List[Throwable])] = {
    @tailrec
    def loop(list: List[Future[A]], success: List[A], failure: List[Throwable]):(List[A], List[Throwable]) = {
      list match {
        case head :: tail => {
          while (!head.isCompleted){
            Thread.sleep(30)
          }
          head.value match {
            case Some(res) => res match {
              case Success(value) => loop(tail, success.appended(value), failure)
              case Failure(exception) => loop(tail, success, failure.appended(exception))
            }
          }
        }
        case Nil => (success, failure)
      }
    }
    Future{
      loop(futures, List(), List())
    }
  }


}
