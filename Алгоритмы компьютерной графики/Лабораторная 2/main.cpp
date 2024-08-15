#include <iostream>
#include "GL/freeglut.h"
#include "GL/gl.h"
#include "GLFW/glfw3.h"
#include "cmath"
using namespace std;
int anglez = 0;
int anglex = 0;
int angley = 0;
bool PolyFace = true;

void ViewCube(){
    glBegin(GL_POLYGON);
    // Нижняя грань
    glColor3f(1, 0, 0);
    glVertex3f(0.2, -0.2, 0.2);
    glVertex3f(0.2, -0.2, -0.2);
    glVertex3f(-0.2, -0.2, -0.2);
    glVertex3f(-0.2, -0.2, 0.2);
    glEnd();

    // перед
    glBegin(GL_POLYGON);
    glColor3f(0, 1, 0);
    glVertex3f(-0.2, -0.2, 0.2);
    glVertex3f(0.2, -0.2, 0.2);
    glVertex3f(0.2, 0.2, 0.2);
    glVertex3f(-0.2, 0.2, 0.2);
    glEnd();
    // Правая грань
    glBegin(GL_POLYGON);
    glColor3f(0, 0, 1);
    glVertex3f(0.2, -0.2, 0.2);
    glVertex3f(0.2, -0.2, -0.2);
    glVertex3f(0.2, 0.2, -0.2);
    glVertex3f(0.2, 0.2, 0.2);

    glEnd();
    // Верхняя грань
    glBegin(GL_POLYGON);
    glColor3f(0, 1, 1);
    glVertex3f(0.2, 0.2, 0.2);
    glVertex3f(0.2, 0.2, -0.2);
    glVertex3f(-0.2, 0.2, -0.2);
    glVertex3f(-0.2, 0.2, 0.2);
    glEnd();
    // Задняя грань
    glBegin(GL_POLYGON);
    glColor3f(1, 0, 1);
    glVertex3f(0.2, -0.2, -0.2);
    glVertex3f(0.2, 0.2, -0.2);
    glVertex3f(-0.2, 0.2, -0.2);
    glVertex3f(-0.2, -0.2, -0.2);
    glEnd();
    // Левая грань
    glBegin(GL_POLYGON);
    glColor3f(1, 1, 0.4);
    glVertex3f(-0.2, -0.2, 0.2);
    glVertex3f(-0.2, 0.2, 0.2);
    glVertex3f(-0.2, 0.2, -0.2);
    glVertex3f(-0.2, -0.2, -0.2);
    glEnd();
}

void display(GLFWwindow *window) {
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
    glLoadIdentity();
    glEnable(GL_DEPTH_TEST);

    glClearColor(0.1, 0.2, 0.4, 0.0);


    GLfloat rotateCubeX[] = {
            1.0f, 0.0f, 0.0f, 0.0f,
            0.0f, static_cast<GLfloat>(cos(anglex * M_PI / 180)), -static_cast<GLfloat>(sin(anglex * M_PI / 180)), 0.0f,
            0.0f, static_cast<GLfloat>(sin(anglex * M_PI / 180)), static_cast<GLfloat>(cos(anglex * M_PI / 180)), 0.0f,
            0.0f, 0.0f, 0.0f, 1.0f
    };

    GLfloat rotateCubeY[] = {
            static_cast<GLfloat>(cos(angley * M_PI / 180)), 0.0f, static_cast<GLfloat>(sin(angley * M_PI / 180)), 0.0f,
            0.0f, 1.0f, 0.0f, 0.0f,
            -static_cast<GLfloat>(sin(angley * M_PI / 180)), 0.0f, static_cast<GLfloat>(cos(angley * M_PI / 180)), 0.0f,
            0.0f, 0.0f, 0.0f, 1.0f
    };

    GLfloat rotateCubeZ[] = {
            static_cast<GLfloat>(cos(anglez * M_PI / 180)), -static_cast<GLfloat>(sin(anglez * M_PI / 180)), 0.0f, 0.0f,
            static_cast<GLfloat>(sin(anglez * M_PI / 180)), static_cast<GLfloat>(cos(anglez * M_PI / 180)), 0.0f, 0.0f,
            0.0f, 0.0f, 1.0f, 0.0f,
            0.0f, 0.0f, 0.0f, 1.0f
    };

    glPushMatrix();
    glMatrixMode(GL_MODELVIEW);

    GLfloat translateCube[] = {
            1.0f, 0.0f, 0.0f, 0.0f,
            0.0f, 1.0f, 0.0f, 0.0f,
            0.0f, 0.0f, 1.0f, 0.0f,
            -0.5f, 0.5f, 0.0f, 1.0f
    };

    glMultMatrixf(translateCube);
    glMultMatrixf(rotateCubeX);
    glMultMatrixf(rotateCubeY);
    glMultMatrixf(rotateCubeZ);

    ViewCube(); //Обычный куб в левом верхнем углу

    glPopMatrix();

    glLoadIdentity();

    glPushMatrix();

    GLfloat translateProection1[] = {
            1.0f, 0.0f, 0.0f, 0.0f,
            0.0f, 1.0f, 0.0f, 0.0f,
            0.0f, 0.0f, 1.0f, 0.0f,
            -0.5f, -0.5f, 0.0f, 1.0f
    };

    glMultMatrixf(translateProection1);

    GLfloat ProectionUp[] = {
            1.0f, 0.0f, 0.0f, 0.0f,
            0.0f, 0.0f, -1.0f, 0.0f,
            0.0f, 1.0f, 0.0f, 0.0f,
            0.0f, 0.0f, 0.0f, 1.0f
    };

    glMultMatrixf(ProectionUp);
    glMultMatrixf(rotateCubeX);
    glMultMatrixf(rotateCubeY);
    glMultMatrixf(rotateCubeZ);

    ViewCube(); //Проекция сверху

    glPopMatrix();

    glLoadIdentity();
    glPushMatrix();

    GLfloat translateProection2[] = {
            1.0f, 0.0f, 0.0f, 0.0f,
            0.0f, 1.0f, 0.0f, 0.0f,
            0.0f, 0.0f, 1.0f, 0.0f,
            0.5f, 0.5f, 0.0f, 1.0f
    };

    glMultMatrixf(translateProection2);

    GLfloat ProectionBok[] = {
            0.0f, 0.0f, 1.0f, 0.0f,
            0.0f, 1.0f, 0.0f, 0.0f,
            -1.0f, 0.0f, 0.0f, 0.0f,
            0.0f, 0.0f, 0.0f, 1.0f
    };

    glMultMatrixf(ProectionBok);
    glMultMatrixf(rotateCubeX);
    glMultMatrixf(rotateCubeY);
    glMultMatrixf(rotateCubeZ);
    ViewCube();// Проекция слева
    glPopMatrix();

    glLoadIdentity();
    glPushMatrix();
    GLfloat translateProection3[] = {
            1.0f, 0.0f, 0.0f, 0.0f,
            0.0f, 1.0f, 0.0f, 0.0f,
            0.0f, 0.0f, 1.0f, 0.0f,
            0.5f, -0.5f, 0.0f, 1.0f
    };

    glMultMatrixf(translateProection3);

    GLfloat ProectionFront[] = {
            -1.0f, 0.0f, 0.0f, 0.0f,
            0.0f, 1.0f, 0.0f, 0.0f,
            0.0f, 0.0f, -1.0f, 0.0f,
            0.0f, 0.0f, 0.0f, 1.0f
    };

    glMultMatrixf(ProectionFront);
    glMultMatrixf(rotateCubeX);
    glMultMatrixf(rotateCubeY);
    glMultMatrixf(rotateCubeZ);

    ViewCube(); // Проекция сзади
    glPopMatrix();

    glFlush();
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

            case GLFW_KEY_LEFT_CONTROL:
                PolyFace = not PolyFace;
                if (PolyFace) {
                    glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
                } else {
                    glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
                }
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
    GLFWwindow *window;

    window = glfwCreateWindow(840, 840, "Laba 2", NULL, NULL);
    glfwMakeContextCurrent(window);

    glfwSetKeyCallback(window, CallbackKeys);
    glfwSetScrollCallback(window, CallbackScroll);
    while (not glfwWindowShouldClose(window)) {
        display(window);
    }

    glfwDestroyWindow(window);
    glfwTerminate();
    return 0;
}
