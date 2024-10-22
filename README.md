## Digital Voting System – An Experimental Implementation

This project showcases a basic implementation of a digital voting system, inspired by an article on [The Software Scaffold](https://iugori.ro/?p=13).
It aims to explore best practices in building modern applications using Spring Boot for the backend and React.js for the frontend.

⚠️ Note: This project is a work in progress, and some features are still under development.

A quick list of technical features used in back-end implemementation:
- Java 21
- Spring Boot 3.4.4
- REST best practices
- HAL compatibility
- HATEOAS support
- Open API & Swagger UI
- Bean Validation framework
  - custom validators
- AOP validation
- Request ID based logging and error reporting
- Uniform REST response error messages (overriding default Spring Boot responses)
- Application error codes
- JPA - Criteria API & Entity Metamodel
- Dynamic generation of queries using Criteia API with support for
  - projection
  - selection
  - sorting
  - pagination
- Unit testing 
  - ArchUnit
  - @DataJpaTest
  - AssertJ
  - REST Assured with custom matchest
- Lombok Project
