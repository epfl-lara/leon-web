package leon.web
package models

case class Link(value: String)
case class Code(value: String)
case class Permalink(link: Link, code: Code)

