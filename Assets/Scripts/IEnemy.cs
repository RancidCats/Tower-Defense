using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public interface IEnemy
{
    void Dead();
    void Move();
    void NextWaypoint();
    void TakeDamage(float damage);
}
