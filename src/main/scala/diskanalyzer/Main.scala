package diskanalyzer

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

object Main {
  def main(args: Array[String]): Unit = {
    // путь к конфигурации можно передать первым аргументом
    val configPath = Paths.get(if (args.nonEmpty) args(0) else "config.json")
    // если файла нет — создаём файл с настройками по умолчанию
    if (!Files.exists(configPath)) {
      try {
        Files.write(configPath, Config.DefaultJson.getBytes(StandardCharsets.UTF_8))
        Term.println(s"Файл конфигурации не найден. Создан файл с настройками по умолчанию: ${configPath.toAbsolutePath}")
      } catch {
        case e: Exception => Term.println(s"Не удалось создать файл конфигурации: ${e.getMessage}")
      }
    }
    Config.load(configPath) match {
      case Left(errors) =>
        val logger = new Logger(Config.DefaultLogFile)
        logger.info("Приложение запущено")
        Term.println(s"Ошибка в файле конфигурации ${configPath.toAbsolutePath}:")
        errors.foreach(e => Term.println(s"  - $e"))
        errors.foreach(e => logger.error(s"Ошибка конфигурации: $e"))
        Term.println("Исправьте файл и запустите программу снова.")
        logger.info("Приложение завершено из-за ошибки конфигурации")
        logger.close()
      case Right(config) =>
        val logger = new Logger(config.logFile)
        logger.info("Приложение запущено")
        logger.info("Конфигурация успешно загружена")
        new App(config, configPath, logger).run()
        logger.info("Приложение завершено")
        logger.close()
        Term.println("До свидания!")
    }
  }
}
