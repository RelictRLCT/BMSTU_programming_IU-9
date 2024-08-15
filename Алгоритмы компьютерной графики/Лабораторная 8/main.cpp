#include <GL/glew.h>
#include <GLFW/glfw3.h>
#include <glm/glm.hpp>
#include <glm/gtc/matrix_transform.hpp>
#include <glm/gtc/type_ptr.hpp>
#include <iostream>

using namespace std;

const char* vertexShaderSource = R"glsl(
#version 330 core
layout (location = 0) in vec3 aPos;
layout (location = 1) in vec3 aColor;

out vec3 ourColor;

uniform mat4 model;
uniform mat4 view;
uniform mat4 projection;

void main() {
    gl_Position = projection * view * model * vec4(aPos, 1.0);
    ourColor = aColor;
}
)glsl"; //вершинный шейдер

const char* fragmentShaderSource = R"glsl(
#version 330 core
out vec4 FragColor;
in vec3 ourColor;

void main() {
    FragColor = vec4(ourColor, 1.0);
}
)glsl";//фрагментный шейдер, который закрасит пиксели цветом вершин

unsigned int compileShader(unsigned int type, const char* source) {
    unsigned int shader = glCreateShader(type);
    glShaderSource(shader, 1, &source, nullptr);
    glCompileShader(shader);
    int success;
    char infoLog[512];
    glGetShaderiv(shader, GL_COMPILE_STATUS, &success);
    if (!success) {
        glGetShaderInfoLog(shader, 512, nullptr, infoLog);
        cerr << "Ошибка компиляции шейдера: " << endl << infoLog << endl;
    }
    return shader;
}

unsigned int createShaderProgram() {
    unsigned int vertexShader = compileShader(GL_VERTEX_SHADER, vertexShaderSource);
    unsigned int fragmentShader = compileShader(GL_FRAGMENT_SHADER, fragmentShaderSource);

    unsigned int shaderProgram = glCreateProgram();
    glAttachShader(shaderProgram, vertexShader);
    glAttachShader(shaderProgram, fragmentShader);
    glLinkProgram(shaderProgram);

    int success;

    glGetProgramiv(shaderProgram, GL_LINK_STATUS, &success);

    glDeleteShader(vertexShader);
    glDeleteShader(fragmentShader);

    return shaderProgram;
}

float anglex = 0;
float angley = 0;
float anglez = 0;

void display(GLFWwindow *window, unsigned int shaderProgram, unsigned int VAO) {

    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
    glClearColor(0.1, 0.2, 0.4, 0.0);
    glUseProgram(shaderProgram);

    // Создание преобразований
    glm::mat4 model = glm::mat4(1.0f);
    model = glm::rotate(model, glm::radians(anglex), glm::vec3(1.0f, 0.0f, 0.0f));
    model = glm::rotate(model, glm::radians(angley), glm::vec3(0.0f, 1.0f, 0.0f));
    model = glm::rotate(model, glm::radians(anglez), glm::vec3(0.0f, 0.0f, 1.0f));

    glm::mat4 view1 = glm::translate(glm::mat4(1.0f), glm::vec3(-0.5f, 0.5f, -2.0f)); //перенос основного куба в левый верхний угол
    glm::mat4 projection1 = glm::ortho(-1.0f, 1.0f, -1.0f, 1.0f, 0.1f, 100.0f);

    glm::mat4 view2 = glm::lookAt(glm::vec3(-1.0f, 0.0f, 0.0f), glm::vec3(0.0f, 0.0f, 0.0f), glm::vec3(0.0f, 1.0f, 0.0f));
    glm::mat4 projection2 = glm::ortho(-1.0f, 1.0f, -1.0f, 1.0f, 0.1f, 100.0f);
    view2 = glm::translate(view2, glm::vec3(0.0f, 0.5f, 0.5f));//перенос проекции куба

    glm::mat4 view3 = glm::lookAt(glm::vec3(0.0f, 1.0f, 0.0f), glm::vec3(0.0f, 0.0f, 0.0f), glm::vec3(0.0f, 0.0f, -1.0f));
    glm::mat4 projection3 = glm::ortho(-1.0f, 1.0f, -1.0f, 1.0f, 0.1f, 100.0f);
    view3 = glm::translate(view3, glm::vec3(-0.5f, 0.0f, 0.5f));

    glm::mat4 view4 = glm::lookAt(glm::vec3(0.0f, 0.0f, -1.0f), glm::vec3(0.0f, 0.0f, 0.0f), glm::vec3(0.0f, 1.0f, 0.0f));
    glm::mat4 projection4 = glm::ortho(-1.0f, 1.0f, -1.0f, 1.0f, 0.1f, 100.0f);
    view4 = glm::translate(view4, glm::vec3(-0.5f, -0.5f, 0.0f));


    // Передача матриц в шейдеры
    unsigned int modelLoc = glGetUniformLocation(shaderProgram, "model");

    glUniformMatrix4fv(modelLoc, 1, GL_FALSE, glm::value_ptr(model));

    unsigned int viewLoc = glGetUniformLocation(shaderProgram, "view");
    unsigned int projectionLoc = glGetUniformLocation(shaderProgram, "projection");


    glUniformMatrix4fv(viewLoc, 1, GL_FALSE, glm::value_ptr(view1));
    glUniformMatrix4fv(projectionLoc, 1, GL_FALSE, glm::value_ptr(projection1));
    glBindVertexArray(VAO);
    glDrawElements(GL_TRIANGLES, 36, GL_UNSIGNED_INT, 0);

    glUniformMatrix4fv(viewLoc, 1, GL_FALSE, glm::value_ptr(view2));
    glUniformMatrix4fv(projectionLoc, 1, GL_FALSE, glm::value_ptr(projection2));
    glBindVertexArray(VAO);
    glDrawElements(GL_TRIANGLES, 36, GL_UNSIGNED_INT, 0);

    glUniformMatrix4fv(viewLoc, 1, GL_FALSE, glm::value_ptr(view3));
    glUniformMatrix4fv(projectionLoc, 1, GL_FALSE, glm::value_ptr(projection3));
    glBindVertexArray(VAO);
    glDrawElements(GL_TRIANGLES, 36, GL_UNSIGNED_INT, 0);

    glUniformMatrix4fv(viewLoc, 1, GL_FALSE, glm::value_ptr(view4));
    glUniformMatrix4fv(projectionLoc, 1, GL_FALSE, glm::value_ptr(projection4));
    glBindVertexArray(VAO);
    glDrawElements(GL_TRIANGLES, 36, GL_UNSIGNED_INT, 0);

    glfwSwapBuffers(window);
    glfwPollEvents();
}

void CallbackKeys(GLFWwindow* window, int key, int scancode, int action, int mods) {

    if (action == GLFW_PRESS) {
        switch (key) {
            case GLFW_KEY_RIGHT:
                angley += 6;
                break;

            case GLFW_KEY_LEFT:
                angley -= 6;
                break;

            case GLFW_KEY_UP:
                anglex -= 6;
                break;

            case GLFW_KEY_DOWN:
                anglex += 6;
                break;

        }
    }
}

void CallbackScroll(GLFWwindow* window, double xoffset, double yoffset) {

    if (yoffset > 0) {
        anglez += 3;
    } else {
        anglez -=3;
    }
}

int main() {
    glfwInit();
    glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 3);
    glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 3);
    glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);

    GLFWwindow* window = glfwCreateWindow(840, 840, "Lab 8", NULL, NULL);
    glfwMakeContextCurrent(window);
    glewInit();

    unsigned int shaderProgram = createShaderProgram();

    float vertices[] = {
            // Координаты                     // Цвета
            -0.2f, -0.2f, -0.2f,  1.0f, 0.0f, 0.0f,  // зад (красный)
            0.2f, -0.2f, -0.2f,  1.0f, 0.0f, 0.0f,
            0.2f,  0.2f, -0.2f,  1.0f, 0.0f, 0.0f,
            -0.2f,  0.2f, -0.2f,  1.0f, 0.0f, 0.0f,

            -0.2f, -0.2f,  0.2f,  0.0f, 1.0f, 0.0f,  // перед (зеленый)
            0.2f, -0.2f,  0.2f,  0.0f, 1.0f, 0.0f,
            0.2f,  0.2f,  0.2f,  0.0f, 1.0f, 0.0f,
            -0.2f,  0.2f,  0.2f,  0.0f, 1.0f, 0.0f,

            -0.2f, -0.2f, -0.2f,  0.0f, 0.0f, 1.0f,  // лево (синий)
            -0.2f,  0.2f, -0.2f,  0.0f, 0.0f, 1.0f,
            -0.2f,  0.2f,  0.2f,  0.0f, 0.0f, 1.0f,
            -0.2f, -0.2f,  0.2f,  0.0f, 0.0f, 1.0f,

            0.2f, -0.2f, -0.2f,  1.0f, 1.0f, 0.0f,  // право (желтый)
            0.2f,  0.2f, -0.2f,  1.0f, 1.0f, 0.0f,
            0.2f,  0.2f,  0.2f,  1.0f, 1.0f, 0.0f,
            0.2f, -0.2f,  0.2f,  1.0f, 1.0f, 0.0f,

            -0.2f, -0.2f, -0.2f,  1.0f, 0.0f, 1.0f,  // низ (розовый)
            0.2f, -0.2f, -0.2f,  1.0f, 0.0f, 1.0f,
            0.2f, -0.2f,  0.2f,  1.0f, 0.0f, 1.0f,
            -0.2f, -0.2f,  0.2f,  1.0f, 0.0f, 1.0f,

            -0.2f,  0.2f, -0.2f,  0.0f, 1.0f, 1.0f,  // верх (голубой)
            0.2f,  0.2f, -0.2f,  0.0f, 1.0f, 1.0f,
            0.2f,  0.2f,  0.2f,  0.0f, 1.0f, 1.0f,
            -0.2f,  0.2f,  0.2f,  0.0f, 1.0f, 1.0f,
    };

    unsigned int indices[] = {
            0, 1, 2, 2, 3, 0,  // зад
            4, 5, 6, 6, 7, 4,  // перед
            8, 9, 10, 10, 11, 8,  // лево
            12, 13, 14, 14, 15, 12,  // право
            16, 17, 18, 18, 19, 16,  // низ
            20, 21, 22, 22, 23, 20   // верх
    };

    unsigned int VBO, VAO, EBO;
    glGenVertexArrays(1, &VAO);
    glGenBuffers(1, &VBO);
    glGenBuffers(1, &EBO);

    glBindVertexArray(VAO);

    // Копирование данных о вершинах в буфер
    glBindBuffer(GL_ARRAY_BUFFER, VBO);
    glBufferData(GL_ARRAY_BUFFER, sizeof(vertices), vertices, GL_STATIC_DRAW);

    // Копирование индексов в буфер
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, EBO);
    glBufferData(GL_ELEMENT_ARRAY_BUFFER, sizeof(indices), indices, GL_STATIC_DRAW);

    // Задание атрибутов вершин
    glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 6 * sizeof(float), (void*)0);
    glEnableVertexAttribArray(0);
    glVertexAttribPointer(1, 3, GL_FLOAT, GL_FALSE, 6 * sizeof(float), (void*)(3 * sizeof(float)));
    glEnableVertexAttribArray(1);

    glBindBuffer(GL_ARRAY_BUFFER, 0);
    glBindVertexArray(0);

    glEnable(GL_DEPTH_TEST);

    glfwSetKeyCallback(window, CallbackKeys);
    glfwSetScrollCallback(window, CallbackScroll);
    while (!glfwWindowShouldClose(window)) {
        display(window, shaderProgram, VAO);
    }

    glDeleteVertexArrays(1, &VAO);
    glDeleteBuffers(1, &VBO);
    glDeleteBuffers(1, &EBO);
    glDeleteProgram(shaderProgram);

    glfwTerminate();
    return 0;
}
